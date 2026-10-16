module Heos.Connection.Internal where

import Control.Concurrent (MVar, newMVar, threadDelay, withMVar)
import Control.Concurrent.Async (concurrently, race)
import Control.Concurrent.STM (TChan, TMVar, TVar, atomically, dupTChan, modifyTVar', newBroadcastTChanIO, newEmptyTMVarIO, newTVarIO, putTMVar, readTVar, stateTVar, takeTMVar, tryPutTMVar, writeTChan, writeTVar)
import Control.Exception (Exception, IOException, bracket, bracket_, finally, throwIO, try)
import Control.Monad (forM_, forever, unless, when)
import Data.Aeson (eitherDecodeStrict)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void, absurd)
import GHC.Exception (displayException)
import Heos.Command (Command (HeartBeat), renderCommand)
import Heos.Envelope (Envelope (..), HeosEvent (..), HeosHeader (..), IncomingEnvelope (..), classifyEnvelope)
import Heos.Url (decodeQuery, lookupParam)
import Network.Socket (Family (AF_INET), SockAddr, Socket, SocketType (Stream), close, connect, defaultProtocol, socket)
import Network.Socket.ByteString (recv, sendAll)
import System.Timeout (timeout)
import Text.Read (readMaybe)

data ConnectionError = ConnectionLost | ConnectionTimeout deriving (Show, Eq)
instance Exception ConnectionError where
  displayException ConnectionLost = "the device closed the connection"
  displayException ConnectionTimeout = "the device did not answer in time"

type EitherEnvelope = Either ConnectionError Envelope

newtype Seconds = Seconds Double deriving (Show, Eq)

toMicros :: Seconds -> Int
toMicros (Seconds s) = round (s * 1000000)

-- | Split a chunk of the socket stream into complete lines and the unfinished
--   tail. Lines are delimited by **\\r\\n**, which is dropped. The tail has no
--   delimiter yet: keep it and prepend it to the next chunk.
splitLines :: ByteString -> ([ByteString], ByteString)
splitLines bs
  | BS.null after = ([], line)
  -- add 'line' to the first element of the 'splitLines' function call result (a tuple)
  | otherwise = first (line :) (splitLines next)
 where
  delimiter = "\r\n"
  (line, after) = BS.breakSubstring delimiter bs
  next = BS.drop (BS.length delimiter) after -- the stream after the delimiter

-- TODO: перенести эти константы в конфиг
data ConnectionParams = ConnectionParams
  { commandTimeout :: Seconds
  , heartbeatDelay :: Seconds
  }
connectionParams :: ConnectionParams
connectionParams =
  ConnectionParams
    { commandTimeout = Seconds 5
    , heartbeatDelay = Seconds 30
    }

-- | A live link to one HEOS device. Shared by every thread that talks to
--   the device; the mutable parts are cells, the record itself never changes.
data Connection = Connection
  { sock :: Socket
  , events :: TChan HeosEvent
  -- ^ broadcast: the reader writes, subscribers read their own copies via 'subscribe'
  , waiting :: TVar (Map Int (TMVar EitherEnvelope))
  -- ^ commands in flight, by SEQUENCE: where the reader puts each reply
  , nextSeq :: TVar Int
  -- ^ the one SEQUENCE counter for this connection
  , sendLock :: MVar ()
  -- ^ one command in flight at a time; see 'sendCommand'
  , params :: ConnectionParams
  }

-- | Open a TCP connection to the device at the given address and run the
--   action over it. The socket is closed when the action returns, throws
--   or is cancelled. Everything but the socket is 'runConnection'.
withConnection :: SockAddr -> (Connection -> IO a) -> IO a
withConnection sockAddr act = bracket open close go
 where
  open = socket AF_INET Stream defaultProtocol
  go sock = do
    connect sock sockAddr
    runConnection connectionParams sock act

-- | Run the action over an already connected socket: start the reader
--   thread, hand out the 'Connection', and kill the reader when the action
--   is done. If the reader dies first (the device closed the connection),
--   the action is cancelled and the reader's error propagates. Takes a
--   socket rather than an address so tests can pass one end of a socket pair.
runConnection :: ConnectionParams -> Socket -> (Connection -> IO a) -> IO a
runConnection params sock act = do
  events <- newBroadcastTChanIO
  waiting <- newTVarIO Map.empty
  nextSeq <- newTVarIO @Int 0
  sendLock <- newMVar ()
  let conn = Connection{sock, events, waiting, nextSeq, sendLock, params}
  res <- race (heartBeat conn `concurrently` reader conn) (act conn)
  either (absurd . fst) pure res

-- | The reader thread: the only one that reads the socket. Cuts the stream
--   into lines, decodes each and routes it: events go to subscribers,
--   replies go to the box 'findBox' picks, "command under process" notices
--   are dropped. A reply with no box to go to is a stray (late, or
--   unsolicited): it is logged and dropped, never kept for the next command.
--   Never returns; an empty recv means the device closed the connection,
--   and that is an error, not a reason to spin.
reader :: Connection -> IO Void
reader conn = go "" `finally` failAll
 where
  go buffer = do
    chunk <- recv conn.sock 4096
    -- пустой `recv` — это конец потока; бросить исключение, а не продолжать
    when (BS.null chunk) $ throwIO ConnectionLost
    let (complete, rest) = splitLines (buffer <> chunk)
    forM_ complete $ \bs -> do
      let eitherIncEnv = classifyEnvelope <$> eitherDecodeStrict bs
      case eitherIncEnv of
        Right (IncomingEvent event _) -> atomically . writeTChan conn.events $ event
        Right (IncomingPending _) -> pure ()
        Right (IncomingResponse _ env) -> do
          delivered <- atomically $ do
            mapIntBox <- readTVar conn.waiting
            let mbBox = findBox env.heos.message mapIntBox
            maybe (pure False) (\box -> True <$ putTMVar box (Right env)) mbBox
          unless delivered $ putStrLn ("stray reply: " <> show env)
        Left err -> print $ show bs <> err
    go rest
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
subscribe :: Connection -> IO (TChan HeosEvent)
subscribe conn = atomically $ dupTChan conn.events

-- | Send a command and wait for its reply. Takes 'sendLock' for the whole
--   exchange, so commands go one at a time: replies without SEQUENCE
--   (get_players, heart_beat) can then only mean the one in flight. The box
--   is registered under the SEQUENCE number before sending, otherwise a
--   fast reply could arrive before anyone waits for it, and removed by the
--   bracket however the wait ends: reply, timeout or cancellation.
sendCommand :: Connection -> Command -> IO EitherEnvelope
sendCommand conn cmd = withMVar conn.sendLock $ \_ -> do
  seqNo <- atomically $ stateTVar conn.nextSeq (\n -> (n, n + 1))
  box <- newEmptyTMVarIO
  bracket_ (open seqNo box) (closeBox seqNo) (go seqNo box)
 where
  -- кладём в карту пустую ячейку под будущий ответ от устройства
  open seqNo box = atomically $ modifyTVar' conn.waiting (Map.insert seqNo box)
  -- в завершении или при обрыве соединения - удалить ячейку
  closeBox seqNo = atomically $ modifyTVar' conn.waiting (Map.delete seqNo)
  go seqNo box =
    try @IOException (sendAll conn.sock $ renderCommand seqNo cmd) >>= \case
      Left _ -> pure $ Left ConnectionLost
      Right _ -> fromMaybe (Left ConnectionTimeout) <$> timeout (toMicros conn.params.commandTimeout) (atomically $ takeTMVar box)

heartBeat :: Connection -> IO Void
heartBeat conn =
  forever $
    threadDelay (toMicros conn.params.heartbeatDelay) >> sendCommand conn HeartBeat >>= \case
      Left err -> throwIO err
      Right _ -> pure ()
