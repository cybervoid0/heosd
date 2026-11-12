module Heos.Connection.Heos where

import Control.Concurrent (newMVar, threadDelay, withMVar)
import Control.Concurrent.Async (
  Concurrently (Concurrently, runConcurrently),
  race,
 )
import Control.Concurrent.STM (
  TChan,
  TMVar,
  atomically,
  dupTChan,
  modifyTVar',
  newBroadcastTChanIO,
  newEmptyTMVarIO,
  newTVarIO,
  readTVar,
  takeTMVar,
  tryPutTMVar,
  writeTChan,
  writeTVar,
 )
import Control.Exception (
  Exception (fromException),
  IOException,
  SomeAsyncException (SomeAsyncException),
  SomeException,
  bracket,
  bracket_,
  catch,
  finally,
  handle,
  throwIO,
 )
import Control.Monad (forM_, forever, unless, when)
import Data.Aeson (eitherDecodeStrict)
import Data.ByteString qualified as BS
import Data.Foldable (asum)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void, absurd)
import Heos.Command.Heos (Command (..), commandParts, decodeReply, renderCommand)
import Heos.Connection.Framing (Pending (..), emptyPending, feedChunk)
import Heos.Connection.Types (
  ConnectionProps (..),
  ConnectionSockets (ConnectionSockets, commands),
  EitherEnvelope,
  HeosConnection (..),
  HeosConnectionParams (..),
  Waiting (..),
 )
import Heos.Envelope.Heos (
  HeosEvent (..),
  HeosHeader (..),
  HeosResult (..),
  IncomingEnvelope (..),
  classifyEnvelope,
  findError,
 )
import Heos.Error (CommandError (..), ConnectionError (..))
import Heos.Time (toMicros)
import Heos.Types
import Heos.Url (decodeQuery, lookupParam)
import Network.Socket (Family (..), HostAddress, SockAddr (..), Socket, SocketType (..), close, connect, defaultProtocol, socket)
import Network.Socket.ByteString (recv, sendAll)
import System.Timeout (timeout)
import Text.Read (readMaybe)

readPid :: Socket -> ConnectionProps -> IO PlayerId
readPid connSock props = do
  players :: [Player] <- sendOn connSock props GetPlayers
  player <- maybe (throwIO NoPlayers) pure (listToMaybe players)
  pure player.pid

-- | One TCP socket connected to the address, closed when the action is
--   done, however it ends.
withSocket :: SockAddr -> (Socket -> IO a) -> IO a
withSocket addr act = bracket open close $ \s -> connectOrLost s >> act s
 where
  open = socket AF_INET Stream defaultProtocol
  connectOrLost s = connect s addr `catch` \(_ :: IOException) -> throwIO ConnectionLost

-- | Open both sockets to the device, run the start-up sequence, then the
--   action. Both sockets are closed when the action is done. Everything
--   but the sockets is 'runConnection'.
withConnection :: HeosConnectionParams -> (HeosConnection -> IO a) -> HostAddress -> IO a
withConnection params act hostAddr =
  let sockAddr = SockAddrInet params.port hostAddr
   in withSocket sockAddr $ \eSock ->
        withSocket sockAddr $ \cSock ->
          let sockets = ConnectionSockets hostAddr eSock cSock
           in runConnection params sockets $ \props -> do
                pid <- readPid cSock props
                act HeosConnection{sockets, props, pid}

-- | Run them all; the first to end, or to fail, takes the rest down with it.
together :: [IO Void] -> IO Void
together = runConcurrently . asum . map Concurrently

-- | Run the action over two already connected sockets: create the shared
--   cells, start a reader and a heartbeat per socket, hand out the
--   'Connection'. The background threads are killed when the action is
--   done; if one of them dies first (the device closed a socket), the
--   action is cancelled and the error propagates. Takes sockets rather
--   than an address so tests can pass their own.
runConnection :: HeosConnectionParams -> ConnectionSockets -> (ConnectionProps -> IO a) -> IO a
runConnection params (ConnectionSockets _ eSock cSock) act = do
  events <- newBroadcastTChanIO
  waiting <- newTVarIO Map.empty
  nextSeq <- newTVarIO @Int 0
  sendLock <- newMVar ()
  let props = ConnectionProps{events, waiting, nextSeq, sendLock, params}
      -- rethrow all exceptions in the bg threads (except async) - as Connection Error (Connection Lost)
      dead :: SomeException -> IO a
      dead e
        | Just (SomeAsyncException inner) <- fromException e = throwIO inner
        | Just (ce :: ConnectionError) <- fromException e = throwIO ce
        | otherwise = throwIO (BackgroundFailure e)

      -- endless threads run concurrently
      bg =
        handle dead $
          together
            [ heartBeat eSock props -- heartBeat для событий
            , heartBeat cSock props -- heartBeat для команд
            , reader eSock props -- reader для событий
            , reader cSock props -- reader для команд
            ]
  res <- race bg (act props)
  either absurd pure res

-- | The reader thread for one socket: the only one that reads it. Cuts the
--   stream into lines, decodes each and routes it: events go to
--   subscribers, replies go to the box 'findBox' picks, "command under
--   process" notices are dropped. The same code serves both sockets, since
--   the events socket also gets replies to its own commands. A reply with no box to go to is a stray (late, or
--   unsolicited): it is logged and dropped, never kept for the next command.
--   Never returns; an empty recv means the device closed the connection,
--   and that is an error, not a reason to spin. Cutting the stream into
--   lines is 'feedChunk'; a line that outgrows 'maxLineLength' without
--   ending is a broken link, 'LineTooLong', so the buffer stays bounded.
reader :: Socket -> ConnectionProps -> IO Void
reader sock conn = go emptyPending `finally` failAll
 where
  go pending = do
    chunk <- recv sock 4096
    -- пустой `recv` — это конец потока; бросить исключение, а не продолжать
    when (BS.null chunk) $ throwIO ConnectionLost
    let (complete, pending') = feedChunk pending chunk
    forM_ complete route
    when (pending'.size > conn.params.maxLineLength) $ throwIO LineTooLong
    go pending'
  -- одна полная строка: разобрать и доставить по назначению
  route bs = do
    let eitherIncEnv = classifyEnvelope <$> eitherDecodeStrict bs
    case eitherIncEnv of
      Right (IncomingEvent event _) -> atomically . writeTChan conn.events $ event
      Right (IncomingPending _) -> pure ()
      Right (IncomingResponse _ env) -> do
        delivered <- atomically $ do
          waiting <- readTVar conn.waiting
          let HeosHeader{command, message} = env.heos
              mbBox = findBox command message waiting
          maybe (pure False) (\box -> tryPutTMVar box (Right env)) mbBox
        unless delivered $ putStrLn ("stray reply: " <> show env)
      Left err -> print $ show bs <> err
  -- при любом завершении (`finally`): пройти по всей карте, положить в каждую коробочку `Left ConnectionLost`, карту очистить.
  failAll = atomically $ do
    mapping <- readTVar conn.waiting
    forM_ mapping $ \waiting -> tryPutTMVar waiting.box (Left ConnectionLost)
    writeTVar conn.waiting Map.empty

-- | Which waiting box a reply belongs to. A reply carries SEQUENCE only
--   when the command had arguments of its own (the device echoes them);
--   without it, the reply can only be for the single command in flight,
--   which 'sendLock' guarantees. Nothing means nobody is waiting for it.
findBox :: Text -> Maybe Text -> Map Int Waiting -> Maybe (TMVar EitherEnvelope)
findBox cmdPath mbMsg boxes = (.box) <$> (candidate >>= same)
 where
  candidate = case seqNo of
    Just sn -> Map.lookup sn boxes
    Nothing -> case Map.elems boxes of [w] -> Just w; _ -> Nothing
  same w = if w.path == cmdPath then Just w else Nothing
  seqNo = mbMsg >>= lookupParam "SEQUENCE" . decodeQuery >>= readMaybe . T.unpack

-- | A private copy of the event stream. Each call gets its own copy, so
--   subscribers don't steal events from each other. Only events arriving
--   after the call are seen: subscribe before you ask the device for them.
--
--   The copy is unbounded: every event stays in it until read. Subscribe
--   only from a thread that keeps reading for as long as it holds the
--   channel; a copy that is dropped costs nothing, a copy that is kept but
--   not read grows with every event.
subscribe :: HeosConnection -> IO (TChan HeosEvent)
subscribe conn = atomically $ dupTChan conn.props.events

-- | Send a command over the given socket and wait for its reply; throws
--   'ConnectionError' when there is none. Takes 'sendLock' for the whole
--   exchange, so commands go one at a time: replies without SEQUENCE
--   (get_players, heart_beat) can then only mean the one in flight. The box
--   is registered under the SEQUENCE number before sending, otherwise a
--   fast reply could arrive before anyone waits for it, and removed by the
--   bracket however the wait ends: reply, timeout or cancellation.
sendOn :: Socket -> ConnectionProps -> Command a -> IO a
sendOn sock props cmd = withMVar props.sendLock $ \_ -> do
  -- взять номер и сдвинуть счётчик; modifyTVar' вычисляет n + 1 сразу,
  -- иначе в TVar лежал бы thunk до следующего чтения
  seqNo <- atomically $ readTVar props.nextSeq <* modifyTVar' props.nextSeq (+ 1)
  box <- newEmptyTMVarIO
  let waiting = Waiting{box, path = fst . commandParts $ cmd}
  bracket_ (open seqNo waiting) (closeBox seqNo) (go seqNo box)
 where
  -- кладём в карту пустую ячейку под будущий ответ от устройства
  open seqNo waiting = atomically $ modifyTVar' props.waiting (Map.insert seqNo waiting)
  -- в завершении или при обрыве соединения - удалить ячейку
  closeBox seqNo = atomically $ modifyTVar' props.waiting (Map.delete seqNo)
  go seqNo box = do
    -- заменить IOException на ConnectionLost при отправке байтов в сокет
    handle (\(_ :: IOException) -> throwIO ConnectionLost) $
      sendAll sock (renderCommand seqNo cmd)
    -- `reader` уже получил ответ на эту команду и положил в ячейку, нужно её
    -- достать и вернуть, а в случае если не уложились в таймаут - бросить исключение
    env <- timeout (toMicros props.params.commandTimeout) (atomically $ takeTMVar box) >>= maybe (throwIO NoReply) (either throwIO pure)

    case (env.heos.result, env.heos.message) of
      (Just Fail, mbMsg) -> throwIO (findError (fromMaybe "" mbMsg))
      _ -> decodeReply env cmd

-- | 'sendOn' over the commands socket, for callers outside this module:
--   a connection error comes back as a value, not as an exception.
sendCommand :: HeosConnection -> Command a -> IO a
sendCommand conn = sendOn conn.sockets.commands conn.props

-- | Keep one socket alive: a heart_beat every 'heartbeatDelay'. The device
--   drops a socket that stays silent, so each socket gets its own. A beat
--   without a reply throws, which is how a dead link is noticed.
heartBeat :: Socket -> ConnectionProps -> IO Void
heartBeat sock props =
  forever $
    threadDelay (toMicros props.params.heartbeatDelay) >> sendOn sock props HeartBeat
