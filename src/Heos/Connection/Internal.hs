module Heos.Connection.Internal (withConnection, splitLines, Connection (..)) where

import Control.Concurrent.Async (race)
import Control.Concurrent.STM (TChan, atomically, newBroadcastTChanIO, writeTChan)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Aeson (eitherDecodeStrict)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Void (Void, absurd)
import Heos.Envelope (Envelope, HeosEvent (..), IncomingEnvelope (..), classifyEnvelope)
import Network.Socket (Family (AF_INET), SockAddr, Socket, SocketType (Stream), close, connect, defaultProtocol, socket)
import Network.Socket.ByteString (recv)

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
    events <- newBroadcastTChanIO
    let conn = Connection{sock, events}
    res <- race (reader conn) (act conn)
    either absurd pure res

  reader :: Connection -> IO Void
  reader (Connection sock events) = go' ""
   where
    go' buffer = do
      chunk <- recv sock 4096
      let (complete, rest) = splitLines (buffer <> chunk)
      forM_ complete $ \bs -> do
        let eitherIncEnv = classifyEnvelope <$> eitherDecodeStrict @Envelope bs
        case eitherIncEnv of
          Right (IncomingEvent event _) -> atomically . writeTChan events $ event
          Right (IncomingResponse _ _) -> print bs
          Right (IncomingPending _) -> pure ()
          Left err -> print $ show bs <> err
      go' rest
