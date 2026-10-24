-- | A stand-in for the receiver on the far end of a socket. Answers every
--   command with a success reply that echoes the command's path and query,
--   SEQUENCE included, the way the real device does; so 'findBox' finds the
--   right box and the connection code can't tell it from a real one.
module Heos.FakeDevice (answering, answeringWith, replyTo, commandPath, commandQuery) where

import Control.Monad (forM_, unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Maybe (fromMaybe)
import Heos.Connection.Framing (emptyPending, feedChunk)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)

-- | Read commands and answer each one with the echo reply until the other
--   side hangs up. Every command line is handed to the callback first, so a
--   test can log it.
answering :: (ByteString -> IO ()) -> Socket -> IO ()
answering = answeringWith replyTo

-- | 'answering' with the reply chosen by the test: the function gets the
--   command line and returns the whole reply line, "\r\n" included.
answeringWith :: (ByteString -> ByteString) -> (ByteString -> IO ()) -> Socket -> IO ()
answeringWith reply record sock = go emptyPending
 where
  go pending = do
    chunk <- recv sock 4096
    unless (BS.null chunk) $ do
      let (complete, pending') = feedChunk pending chunk
      forM_ complete $ \line -> do
        record line
        sendAll sock (reply line)
      go pending'

-- | The echo reply: success, with the command's own query as the message.
replyTo :: ByteString -> ByteString
replyTo line =
  "{\"heos\": {\"command\": \"" <> commandPath line <> "\", \"result\": \"success\", \"message\": \"" <> commandQuery line <> "\"}}\r\n"

-- | "heos://player/get_volume?pid=1&SEQUENCE=3" -> "player/get_volume"
commandPath :: ByteString -> ByteString
commandPath = fst . splitUrl

-- | "heos://player/get_volume?pid=1&SEQUENCE=3" -> "pid=1&SEQUENCE=3"
commandQuery :: ByteString -> ByteString
commandQuery = snd . splitUrl

splitUrl :: ByteString -> (ByteString, ByteString)
splitUrl line = (path, BS.drop 1 rest)
 where
  body = fromMaybe line (BS.stripPrefix "heos://" line)
  (path, rest) = BC.break (== '?') body
