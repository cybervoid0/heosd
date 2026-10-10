module Heos.ConnectionSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TChan, atomically, readTChan)
import Control.Exception (bracket)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Text.Encoding qualified as T
import Heos.Connection.Internal (Connection, runConnection, splitLines, subscribe)
import Heos.Envelope (HeosEvent (..))
import Heos.Url (decodeQuery)
import Network.Socket (Family (AF_UNIX), Socket, SocketType (Stream), close, socketPair)
import Network.Socket.ByteString (sendAll)
import System.Timeout (timeout)
import Test.Hspec
import Test.QuickCheck

-- Bytes that make splitting interesting: plain letters plus both halves of
-- the delimiter, so "\r\n", a lone "\r" and a lone "\n" all show up.
genBytes :: Gen ByteString
genBytes = BC.pack <$> listOf (elements "ab\r\n")

-- Cut a string at the given positions, the way recv hands out chunks of
-- arbitrary size.
chunksAt :: [Int] -> ByteString -> [ByteString]
chunksAt [] bs = [bs]
chunksAt (n : ns) bs = let (h, t) = BS.splitAt n bs in h : chunksAt ns t

-- Feed chunks one by one, carrying the remainder, as the read loop of stage 13
-- will do.
feed :: [ByteString] -> ([ByteString], ByteString)
feed = foldl step ([], "")
 where
  step (done, buffer) chunk =
    let (complete, rest) = splitLines (buffer <> chunk)
     in (done <> complete, rest)

-- A pair of connected sockets, no network involved. `ours` goes to
-- runConnection; the test writes into `theirs`, playing the receiver.
withSocketPair :: ((Socket, Socket) -> IO a) -> IO a
withSocketPair = bracket (socketPair AF_UNIX Stream 0) (\(a, b) -> close a >> close b)

-- Wire format: one JSON object per line, "\r\n" at the end.
eventLine :: ByteString -> ByteString -> ByteString
eventLine name msg =
  "{\"heos\": {\"command\": \"event/" <> name <> "\", \"message\": \"" <> msg <> "\"}}\r\n"

responseLine :: ByteString
responseLine =
  "{\"heos\": {\"command\": \"player/volume_up\", \"result\": \"success\", \"message\": \"pid=1&step=1\"}}\r\n"

pendingLine :: ByteString
pendingLine =
  "{\"heos\": {\"command\": \"browse/search\", \"result\": \"success\", \"message\": \"command under process&sid=10\"}}\r\n"

volumeEvent :: ByteString -> HeosEvent
volumeEvent level = HeosEvent "player_volume_changed" (decodeQuery ("pid=1&level=" <> T.decodeUtf8 level))

-- Reading from the channel must not hang the whole suite when the reader
-- is broken: give up after a second.
nextEvent :: TChan HeosEvent -> IO HeosEvent
nextEvent ch =
  timeout 1000000 (atomically (readTChan ch))
    >>= maybe (fail "no event arrived within a second") pure

spec :: Spec
spec = do
  describe "runConnection" $ do
    it "returns what the action returns" $
      withSocketPair $ \(ours, _) ->
        runConnection ours (\_ -> pure (42 :: Int)) `shouldReturn` 42

    it "delivers events to a subscriber in the order they arrived" $
      withSocketPair $ \(ours, theirs) ->
        runConnection ours $ \conn -> do
          ch <- subscribe conn
          sendAll theirs (eventLine "player_volume_changed" "pid=1&level=20")
          sendAll theirs (eventLine "player_volume_changed" "pid=1&level=21")
          nextEvent ch `shouldReturn` volumeEvent "20"
          nextEvent ch `shouldReturn` volumeEvent "21"

    -- Responses and "command under process" are not events: subscribers
    -- must never see them. (The reader currently prints the response to
    -- stdout; that line in the test output is expected.)
    it "passes only events to subscribers, not responses or pending notices" $
      withSocketPair $ \(ours, theirs) ->
        runConnection ours $ \conn -> do
          ch <- subscribe conn
          sendAll theirs responseLine
          sendAll theirs pendingLine
          sendAll theirs (eventLine "player_state_changed" "pid=1&state=play")
          nextEvent ch `shouldReturn` HeosEvent "player_state_changed" (decodeQuery "pid=1&state=play")

    -- recv hands out arbitrary chunks; a line split across two of them is
    -- still one line. Two writes with a pause in between force two recvs.
    it "reassembles an event that arrived in two pieces" $
      withSocketPair $ \(ours, theirs) ->
        runConnection ours $ \conn -> do
          ch <- subscribe conn
          let (firstHalf, secondHalf) = BS.splitAt 30 (eventLine "player_volume_changed" "pid=1&level=7")
          sendAll theirs firstHalf
          threadDelay 50000
          sendAll theirs secondHalf
          nextEvent ch `shouldReturn` volumeEvent "7"

    it "gives every subscriber its own copy of each event" $
      withSocketPair $ \(ours, theirs) ->
        runConnection ours $ \conn -> do
          ch1 <- subscribe conn
          ch2 <- subscribe conn
          sendAll theirs (eventLine "player_volume_changed" "pid=1&level=5")
          nextEvent ch1 `shouldReturn` volumeEvent "5"
          nextEvent ch2 `shouldReturn` volumeEvent "5"

    -- The device closing the connection must end runConnection with an
    -- error, not leave the action waiting forever (and not spin the reader).
    it "fails, and stops the action, when the device closes the connection" $
      withSocketPair $ \(ours, theirs) -> do
        let stuck = runConnection ours $ \(_ :: Connection) -> do
              close theirs
              forever (threadDelay 100000)
        timeout 2000000 stuck `shouldThrow` anyIOException

  describe "splitLines" $ do
    -- The three checks named in the roadmap.
    it "keeps a chunk without a delimiter as the remainder" $
      splitLines "{\"heos\": {\"comm" `shouldBe` ([], "{\"heos\": {\"comm")

    it "returns both lines from a chunk with two complete lines" $
      splitLines "one\r\ntwo\r\n" `shouldBe` (["one", "two"], "")

    it "splits off the unfinished tail of a chunk that ends mid-line" $
      splitLines "one\r\ntwo\r\nthr" `shouldBe` (["one", "two"], "thr")

    it "returns nothing for an empty chunk" $
      splitLines "" `shouldBe` ([], "")

    -- "\r" alone is half a delimiter: the "\n" may be in the next chunk.
    it "keeps a trailing \\r in the remainder" $
      splitLines "one\r" `shouldBe` ([], "one\r")

    it "doesn't treat a bare \\n or \\r as a delimiter" $
      splitLines "a\nb\rc\r\n" `shouldBe` (["a\nb\rc"], "")

    -- An empty line is still a line. HEOS doesn't send them, but splitLines
    -- shouldn't silently drop data either; skipping empties is the caller's call.
    it "reports an empty line between two delimiters" $
      splitLines "one\r\n\r\ntwo\r\n" `shouldBe` (["one", "", "two"], "")

    it "splits a real get_players reply with the start of the next one glued on" $
      splitLines (getPlayers <> "\r\n{\"heos\": {\"command\": \"event/")
        `shouldBe` ([getPlayers], "{\"heos\": {\"command\": \"event/")

    describe "properties" $ do
      it "loses nothing: lines and remainder joined back give the input" $
        forAll genBytes $ \s ->
          let (complete, rest) = splitLines s
           in BS.intercalate "\r\n" (complete <> [rest]) === s

      it "never leaves a delimiter inside a line or the remainder" $
        forAll genBytes $ \s ->
          let (complete, rest) = splitLines s
           in not (any (BS.isInfixOf "\r\n") (rest : complete))

      -- The reason splitLines exists: however recv cuts the stream, carrying the
      -- remainder over must give the same lines as reading it all at once.
      it "gives the same result however the input is cut into chunks" $
        forAll genBytes $ \s ->
          forAll (listOf (choose (0, 5))) $ \cuts ->
            feed (chunksAt cuts s) === splitLines s
 where
  getPlayers =
    "{\"heos\": {\"command\": \"player/get_players\", \"result\": \"success\", \"message\": \"\"}, \"payload\": [{\"name\": \"Marantz MODEL 60n\", \"pid\": -1245882071}]}"
