module Heos.Connection.Internal where

import Control.Concurrent (MVar, newMVar, threadDelay, withMVar)
import Control.Concurrent.Async (Concurrently (..), race)
import Control.Concurrent.STM (TChan, TMVar, TVar, atomically, dupTChan, modifyTVar', newBroadcastTChanIO, newEmptyTMVarIO, newTVarIO, readTVar, takeTMVar, tryPutTMVar, writeTChan, writeTVar)
import Control.Exception (Exception, IOException, bracket, bracket_, finally, handle, throwIO, try)
import Control.Monad (forM_, forever, unless, when)
import Data.Aeson (eitherDecodeStrict)
import Data.ByteString qualified as BS
import Data.Foldable (asum)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void, absurd)
import GHC.Exception (displayException)
import Heos.Command (Command (GetGroups, GetMusicSources, GetNowPlayingMedia, GetPlayState, GetPlayers, GetVolume, HeartBeat, RegisterForChangeEvents), renderCommand)
import Heos.Connection.Framing (Pending (..), emptyPending, feedChunk)
import Heos.Connection.Parameters (ConnectionParams (..), defaultParams)
import Heos.Envelope (
  Envelope (..),
  HeosEvent (..),
  HeosHeader (..),
  IncomingEnvelope (..),
  classifyEnvelope,
 )
import Heos.Player (OnOff (..))
import Heos.Time (toMicros)
import Heos.Url (decodeQuery, lookupParam)
import Network.Socket (Family (AF_INET), SockAddr, Socket, SocketType (Stream), close, connect, defaultProtocol, socket)
import Network.Socket.ByteString (recv, sendAll)
import System.Timeout (timeout)
import Text.Read (readMaybe)

data ConnectionError = ConnectionLost | ConnectionTimeout | LineTooLong deriving (Show, Eq)
instance Exception ConnectionError where
  displayException ConnectionLost = "the device closed the connection"
  displayException ConnectionTimeout = "the device did not answer in time"
  displayException LineTooLong = "the device sent a line longer than allowed"

type EitherEnvelope = Either ConnectionError Envelope

-- | The two TCP sockets to the device, as the vendor recommends: one
--   carries notifications, the other carries commands, so a burst of
--   events never delays a reply.
data ConnectionSockets = ConnectionSockets
  { events :: Socket
  , commands :: Socket
  }

-- | Everything the two sockets share. The cells are created once per
--   connection in 'runConnection'; both readers and every sender use them.
data ConnectionProps = ConnectionProps
  { events :: TChan HeosEvent
  -- ^ broadcast: the reader writes, subscribers read their own copies via 'subscribe'
  , waiting :: TVar (Map Int (TMVar EitherEnvelope))
  -- ^ commands in flight, by SEQUENCE: where the reader puts each reply
  , nextSeq :: TVar Int
  -- ^ the one SEQUENCE counter for this connection
  , sendLock :: MVar ()
  -- ^ one command in flight at a time, across both sockets; see 'sendOn'
  , params :: ConnectionParams
  }

-- | A live link to one HEOS device: the sockets and what they share.
--   Handed to the user's action; abstract outside this module.
data Connection = Connection
  { sockets :: ConnectionSockets
  , props :: ConnectionProps
  }

-- | The start-up sequence the vendor prescribes: events off, the state
--   queries, events on. Subscription commands go over the events socket,
--   queries over the commands socket. A command without a reply throws,
--   so a half-initialized connection never reaches the action.
initialize :: Connection -> IO ()
initialize conn = do
  let (ConnectionSockets eSock cSock) = conn.sockets
      props = conn.props
      playerId = props.params.playerId
      query = sendOn cSock props
      subscribeCmd = sendOn eSock props
  _ <- subscribeCmd (RegisterForChangeEvents Off)
  -- TODO: поставить сюда signIn
  -- TODO этап 30: тут нужно проверить, есть ли плееры, иначе и говорить дальше не о чем. Пустой список бросает ConnectionError. И если нет своего PlayerID — то тоже.
  playersEnv <- query GetPlayers
  groupsEnv <- query GetGroups
  musicSourcesEnv <- query GetMusicSources
  playingMediaEnv <- query (GetNowPlayingMedia playerId)
  volumeEnv <- query (GetVolume playerId)
  playStateEnv <- query (GetPlayState playerId)
  _ <- subscribeCmd (RegisterForChangeEvents On)

  let queryResults = [playersEnv, groupsEnv, musicSourcesEnv, playingMediaEnv, volumeEnv, playStateEnv]
  forM_ queryResults print

-- | One TCP socket connected to the address, closed when the action is
--   done, however it ends.
withSocket :: SockAddr -> (Socket -> IO a) -> IO a
withSocket addr act = bracket open close $ \s -> connect s addr >> act s
 where
  open = socket AF_INET Stream defaultProtocol

-- | Open both sockets to the device, run the start-up sequence, then the
--   action. Both sockets are closed when the action is done. Everything
--   but the sockets is 'runConnection'.
withConnection :: SockAddr -> (Connection -> IO a) -> IO a
withConnection addr act =
  withSocket addr $ \eSock ->
    withSocket addr $ \cSock ->
      runConnection defaultParams (ConnectionSockets eSock cSock) $ \conn -> initialize conn >> act conn

-- | Run them all; the first to end, or to fail, takes the rest down with it.
together :: [IO Void] -> IO Void
together = runConcurrently . asum . map Concurrently

-- | Run the action over two already connected sockets: create the shared
--   cells, start a reader and a heartbeat per socket, hand out the
--   'Connection'. The background threads are killed when the action is
--   done; if one of them dies first (the device closed a socket), the
--   action is cancelled and the error propagates. Takes sockets rather
--   than an address so tests can pass their own.
runConnection :: ConnectionParams -> ConnectionSockets -> (Connection -> IO a) -> IO a
runConnection params sockets@(ConnectionSockets eSock cSock) act = do
  events <- newBroadcastTChanIO
  waiting <- newTVarIO Map.empty
  nextSeq <- newTVarIO @Int 0
  sendLock <- newMVar ()
  let props = ConnectionProps{events, waiting, nextSeq, sendLock, params}
      conn = Connection{sockets, props}
  -- endless threads run concurrently
  let bg =
        together
          [ heartBeat eSock props -- heartBeat для событий
          , heartBeat cSock props -- heartBeat для команд
          , reader eSock props -- reader для событий
          , reader cSock props -- reader для команд
          ]
  res <- race bg (act conn)
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
          mapIntBox <- readTVar conn.waiting
          let mbBox = findBox env.heos.message mapIntBox
          maybe (pure False) (\box -> tryPutTMVar box (Right env)) mbBox
        unless delivered $ putStrLn ("stray reply: " <> show env)
      Left err -> print $ show bs <> err
  -- при любом завершении (`finally`): пройти по всей карте, положить в каждую коробочку `Left ConnectionLost`, карту очистить.
  failAll = atomically $ do
    boxes <- readTVar conn.waiting
    forM_ boxes $ \box -> tryPutTMVar box (Left ConnectionLost)
    writeTVar conn.waiting Map.empty

-- | Which waiting box a reply belongs to. A reply carries SEQUENCE only
--   when the command had arguments of its own (the device echoes them);
--   without it, the reply can only be for the single command in flight,
--   which 'sendLock' guarantees. Nothing means nobody is waiting for it.
findBox :: Maybe Text -> Map Int box -> Maybe box
findBox mbSeqTxt boxes = case seqNo of
  Just sn -> Map.lookup sn boxes -- ответ с номером: его коробочка
  Nothing -> case Map.elems boxes of -- без номера: единственная в полёте
    [box] -> Just box
    _ -> Nothing
 where
  -- вытащить номер SEQUENCE из параметров конверта
  seqNo = mbSeqTxt >>= lookupParam "SEQUENCE" . decodeQuery >>= readMaybe . T.unpack

-- | A private copy of the event stream. Each call gets its own copy, so
--   subscribers don't steal events from each other. Only events arriving
--   after the call are seen: subscribe before you ask the device for them.
--
--   The copy is unbounded: every event stays in it until read. Subscribe
--   only from a thread that keeps reading for as long as it holds the
--   channel; a copy that is dropped costs nothing, a copy that is kept but
--   not read grows with every event.
subscribe :: Connection -> IO (TChan HeosEvent)
subscribe conn = atomically $ dupTChan conn.props.events

-- | Send a command over the given socket and wait for its reply; throws
--   'ConnectionError' when there is none. Takes 'sendLock' for the whole
--   exchange, so commands go one at a time: replies without SEQUENCE
--   (get_players, heart_beat) can then only mean the one in flight. The box
--   is registered under the SEQUENCE number before sending, otherwise a
--   fast reply could arrive before anyone waits for it, and removed by the
--   bracket however the wait ends: reply, timeout or cancellation.
sendOn :: Socket -> ConnectionProps -> Command -> IO Envelope
sendOn sock props cmd = withMVar props.sendLock $ \_ -> do
  -- взять номер и сдвинуть счётчик; modifyTVar' вычисляет n + 1 сразу,
  -- иначе в TVar лежал бы thunk до следующего чтения
  seqNo <- atomically $ readTVar props.nextSeq <* modifyTVar' props.nextSeq (+ 1)
  box <- newEmptyTMVarIO
  bracket_ (open seqNo box) (closeBox seqNo) (go seqNo box)
 where
  -- кладём в карту пустую ячейку под будущий ответ от устройства
  open seqNo box = atomically $ modifyTVar' props.waiting (Map.insert seqNo box)
  -- в завершении или при обрыве соединения - удалить ячейку
  closeBox seqNo = atomically $ modifyTVar' props.waiting (Map.delete seqNo)
  go seqNo box = do
    -- заменить IOException на ConnectionLost при отправке байтов в сокет
    handle (\(_ :: IOException) -> throwIO ConnectionLost) $
      sendAll sock (renderCommand seqNo cmd)
    -- кто-то снаружи уже положил ответ на эту команду в ячейку, нужно её достать
    -- и вернуть, а в случае если не уложились в таймаут - бросить исключение
    timeout (toMicros props.params.commandTimeout) (atomically $ takeTMVar box) >>= maybe (throwIO ConnectionTimeout) (either throwIO pure)

-- | 'sendOn' over the commands socket, for callers outside this module:
--   a connection error comes back as a value, not as an exception.
sendCommand :: Connection -> Command -> IO EitherEnvelope
sendCommand conn cmd = try @ConnectionError $ sendOn conn.sockets.commands conn.props cmd

-- | Keep one socket alive: a heart_beat every 'heartbeatDelay'. The device
--   drops a socket that stays silent, so each socket gets its own. A beat
--   without a reply throws, which is how a dead link is noticed.
heartBeat :: Socket -> ConnectionProps -> IO Void
heartBeat sock props =
  forever $
    threadDelay (toMicros props.params.heartbeatDelay) >> sendOn sock props HeartBeat
