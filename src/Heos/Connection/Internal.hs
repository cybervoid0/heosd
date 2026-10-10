module Heos.Connection.Internal where

import Control.Concurrent.Async (race)
import Control.Concurrent.STM (TChan, atomically, dupTChan, newBroadcastTChanIO, writeTChan)
import Control.Exception (bracket, throwIO)
import Control.Monad (forM_, when)
import Data.Aeson (eitherDecodeStrict)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Void (Void, absurd)
import Heos.Envelope (HeosEvent (..), IncomingEnvelope (..), classifyEnvelope)
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

data Connection = Connection
  { sock :: Socket
  , events :: TChan HeosEvent
  }

withConnection :: SockAddr -> (Connection -> IO a) -> IO a
withConnection sockAddr act = bracket open close go
 where
  open = socket AF_INET Stream defaultProtocol
  go sock = do
    connect sock sockAddr
    runConnection sock act

runConnection :: Socket -> (Connection -> IO a) -> IO a
runConnection sock act = do
  events <- newBroadcastTChanIO
  let conn = Connection{sock, events}
  res <- race (reader conn) (act conn)
  either absurd pure res

reader :: Connection -> IO Void
reader (Connection sock events) = go ""
 where
  go buffer = do
    chunk <- recv sock 4096
    when (BS.null chunk) $ throwIO (userError "connection closed by device")
    let (complete, rest) = splitLines (buffer <> chunk)
    forM_ complete $ \bs -> do
      let eitherIncEnv = classifyEnvelope <$> eitherDecodeStrict bs
      case eitherIncEnv of
        Right (IncomingEvent event _) -> atomically . writeTChan events $ event
        Right (IncomingResponse _ _) -> print bs
        Right (IncomingPending _) -> pure ()
        Left err -> print $ show bs <> err
    go rest

subscribe :: Connection -> IO (TChan HeosEvent)
subscribe conn = atomically $ dupTChan conn.events

sendRaw :: Connection -> ByteString -> IO ()
sendRaw conn = sendAll conn.sock