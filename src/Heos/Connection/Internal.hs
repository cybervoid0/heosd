module Heos.Connection.Internal where

import Control.Concurrent (MVar, newMVar, withMVar)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (TChan, TMVar, TVar, atomically, dupTChan, modifyTVar', newBroadcastTChanIO, newEmptyTMVarIO, newTVarIO, putTMVar, readTVar, stateTVar, takeTMVar, writeTChan)
import Control.Exception (bracket, throwIO)
import Control.Monad (forM_, unless, when)
import Data.Aeson (eitherDecodeStrict)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void, absurd)
import Heos.Command (Command, renderCommand)
import Heos.Envelope (Envelope (..), HeosEvent (..), HeosHeader (..), IncomingEnvelope (..), classifyEnvelope)
import Heos.Url (decodeQuery, lookupParam)
import Network.Socket (Family (AF_INET), SockAddr, Socket, SocketType (Stream), close, connect, defaultProtocol, socket)
import Network.Socket.ByteString (recv, sendAll)
import Text.Read (readMaybe)

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

-- | A live link to one HEOS device. Shared by every thread that talks to
--   the device; the mutable parts are cells, the record itself never changes.
data Connection = Connection
  { sock :: Socket
  , events :: TChan HeosEvent
  -- ^ broadcast: the reader writes, subscribers read their own copies via 'subscribe'
  , waiting :: TVar (Map Int (TMVar Envelope))
  -- ^ commands in flight, by SEQUENCE: where the reader puts each reply
  , nextSeq :: TVar Int
  -- ^ the one SEQUENCE counter for this connection
  , sendLock :: MVar ()
  -- ^ one command in flight at a time; see 'sendCommand'
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
    runConnection sock act

-- | Run the action over an already connected socket: start the reader
--   thread, hand out the 'Connection', and kill the reader when the action
--   is done. If the reader dies first (the device closed the connection),
--   the action is cancelled and the reader's error propagates. Takes a
--   socket rather than an address so tests can pass one end of a socket pair.
runConnection :: Socket -> (Connection -> IO a) -> IO a
runConnection sock act = do
  events <- newBroadcastTChanIO
  waiting <- newTVarIO Map.empty
  nextSeq <- newTVarIO @Int 0
  sendLock <- newMVar ()
  let conn = Connection{sock, events, waiting, nextSeq, sendLock}
  res <- race (reader conn) (act conn)
  either absurd pure res

-- | The reader thread: the only one that reads the socket. Cuts the stream
--   into lines, decodes each and routes it: events go to subscribers,
--   replies go to the box 'findBox' picks, "command under process" notices
--   are dropped. A reply with no box to go to is a stray (late, or
--   unsolicited): it is logged and dropped, never kept for the next command.
--   Never returns; an empty recv means the device closed the connection,
--   and that is an error, not a reason to spin.
reader :: Connection -> IO Void
reader conn = go ""
 where
  go buffer = do
    chunk <- recv conn.sock 4096
    when (BS.null chunk) $ throwIO (userError "connection closed by device")
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
            -- если нужная ячейка найдена, кладём ответ в неё
            maybe (pure False) (\box -> True <$ putTMVar box env) mbBox
          unless delivered $ putStrLn ("stray reply: " <> show env)
        Left err -> print $ show bs <> err
    go rest

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
--   fast reply could arrive before anyone waits for it, and removed in the
--   same transaction that takes the reply out.
sendCommand :: Connection -> Command -> IO Envelope
sendCommand conn cmd = withMVar conn.sendLock $ \_ -> do
  -- взять следующий номер
  seqNo <- atomically $ stateTVar conn.nextSeq (\n -> (n, n + 1))
  -- создать пустую ячейку
  box <- newEmptyTMVarIO
  -- положить созданную ячейку в карту под новым номером
  atomically $ modifyTVar' conn.waiting (Map.insert seqNo box)
  -- послать команду устройству в сокет в виде ByteString
  sendAll conn.sock (renderCommand seqNo cmd)
  -- дождаться пока в ячейке появится значение
  atomically $ do
    env <- takeTMVar box
    -- удалить конверт из карты
    modifyTVar' conn.waiting (Map.delete seqNo)
    pure env
