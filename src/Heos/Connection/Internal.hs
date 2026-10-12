module Heos.Connection.Internal where

import Control.Concurrent.Async (race)
import Control.Concurrent.STM (TChan, TMVar, TVar, atomically, dupTChan, newBroadcastTChanIO, newEmptyTMVarIO, newTVarIO, putTMVar, readTVar, takeTMVar, writeTChan, writeTVar)
import Control.Exception (bracket, throwIO)
import Control.Monad (forM_, unless, when)
import Data.Aeson (eitherDecodeStrict)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Void (Void, absurd)
import Heos.Command (Command, renderCommand)
import Heos.Envelope (Envelope, HeosEvent (..), IncomingEnvelope (..), classifyEnvelope)
import Network.Socket (Family (AF_INET), SockAddr, Socket, SocketType (Stream), close, connect, defaultProtocol, socket)
import Network.Socket.ByteString (recv, sendAll)

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

-- | Who is waiting for the next reply. A reply that arrives while Nobody
--   waits is a stray (late, or unsolicited) and is dropped, never handed
--   to the next command.
data Waiting = Nobody | Someone (TMVar Envelope)

-- | A live link to one HEOS device. Shared by every thread that talks to
--   the device; the mutable parts are cells, the record itself never changes.
data Connection = Connection
  { sock :: Socket
  , events :: TChan HeosEvent
  , waiting :: TVar Waiting
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
  waiting <- newTVarIO Nobody
  let conn = Connection{sock, events, waiting}
  res <- race (reader conn) (act conn)
  either absurd pure res

-- | The reader thread: the only one that reads the socket. Cuts the stream
--   into lines, decodes each and routes it: events go to subscribers,
--   replies go to whoever is 'waiting', "command under process" notices
--   are dropped. Never returns; an empty recv means the device closed the
--   connection, and that is an error, not a reason to spin.
reader :: Connection -> IO Void
reader conn = go ""
 where
  go buffer = do
    chunk <- recv conn.sock 4096
    when (BS.null chunk) $ throwIO (userError "connection closed by device")
    let (complete, rest) = splitLines (buffer <> chunk)
    forM_ complete $ \bs -> do
      -- декодировать JSON и классифицировать тип ответа
      let eitherIncEnv = classifyEnvelope <$> eitherDecodeStrict bs
      case eitherIncEnv of
        -- если пришло событие, положить в очередь событий
        Right (IncomingEvent event _) -> atomically . writeTChan conn.events $ event
        -- если пришёл ответ на команду, открыть ячейку и положить значение туда
        Right (IncomingResponse _ env) -> do
          delivered <- atomically $ do
            -- смотрим кто ждёт ответа
            pending <- readTVar conn.waiting
            case pending of
              -- пришел ответ, которого никто не ждёт? хм, странно
              Nobody -> pure False
              -- ответ кладётся таки в ячейку
              Someone box -> True <$ putTMVar box env
          -- если получен бесхозный ответ, сообщить об этом
          unless delivered $ putStrLn ("stray reply: " <> show env)
        Right (IncomingPending _) -> pure ()
        Left err -> print $ show bs <> err
    go rest

-- | A private copy of the event stream. Each call gets its own copy, so
--   subscribers don't steal events from each other. Only events arriving
--   after the call are seen: subscribe before you ask the device for them.
subscribe :: Connection -> IO (TChan HeosEvent)
subscribe conn = atomically $ dupTChan conn.events

-- | Send a command and wait for its reply. One command in flight at a
--   time: the next reply is assumed to be ours, there is no matching yet
--   (stage 18). The box is registered before sending, otherwise a fast
--   reply could arrive before anyone waits for it.
sendCommand :: Connection -> Command -> Int -> IO Envelope
sendCommand conn cmd seqNo = do
  -- создать пустую ячейку
  box <- newEmptyTMVarIO
  -- подключить ячейку к соединению
  atomically $ writeTVar conn.waiting (Someone box)
  -- послать команду устройству в сокет в виде ByteString
  sendAll conn.sock (renderCommand seqNo cmd)
  -- дождаться пока в ячейке появится значение, после чего вернуть его
  atomically $ do
    env <- takeTMVar box
    -- обнулить ячейку после получения ответа
    writeTVar conn.waiting Nobody
    pure env
