-- | A stand-in for the receiver on the far end of a socket. Answers every
--   command with a success reply that echoes the command's path and query,
--   SEQUENCE included, the way the real device does; so 'findBox' finds the
--   right box and the connection code can't tell it from a real one.
module Heos.FakeDevice (
  answering,
  answeringWith,
  replyTo,
  scripted,
  messageReply,
  failReply,
  playersLine,
  fixture,
  commandPath,
  commandQuery,
) where

import Control.Monad (forM_, unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Maybe (fromMaybe)
import Heos.Connection.Framing (emptyPending, feedChunk)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)
import System.FilePath ((</>))

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

-- | Answer from a table by command path; anything else gets the echo reply.
scripted :: [(ByteString, ByteString)] -> ByteString -> ByteString
scripted table line = fromMaybe (replyTo line) (lookup (commandPath line) table)

-- | A success reply whose message carries the given query, the way
--   get_volume and get_play_state answer.
messageReply :: ByteString -> ByteString -> ByteString
messageReply path msg =
  "{\"heos\": {\"command\": \"" <> path <> "\", \"result\": \"success\", \"message\": \"" <> msg <> "\"}}\r\n"

-- | A fail reply with the given error message, the way the device refuses
--   a command: @eid=14&text=cannot play@.
failReply :: ByteString -> ByteString -> ByteString
failReply path msg =
  "{\"heos\": {\"command\": \"" <> path <> "\", \"result\": \"fail\", \"message\": \"" <> msg <> "\"}}\r\n"

-- | A get_players reply with one player, pid 1: what 'withConnection' needs
--   to find its player. Fits any test that sends its commands to pid 1.
playersLine :: ByteString
playersLine =
  "{\"heos\": {\"command\": \"player/get_players\", \"result\": \"success\", \"message\": \"\"}, \"payload\": [{\"name\": \"Marantz MODEL 60n\", \"pid\": 1, \"model\": \"Marantz MODEL 60n\", \"version\": \"3.139.173\", \"ip\": \"192.168.1.241\", \"network\": \"wifi\", \"lineout\": 0, \"serial\": \"MCQJ072400069\"}]}\r\n"

-- | A recorded reply as one wire line. The files are pretty-printed; only
--   "\r\n" ends a line on the wire, so stray "\r" must go.
fixture :: FilePath -> IO ByteString
fixture file = (<> "\r\n") . BC.filter (/= '\r') <$> BS.readFile ("test/fixtures/responses" </> file)

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
