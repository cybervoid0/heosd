module Heos.ConnectionSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM (TChan, atomically, readTChan, readTVarIO)
import Control.Exception (bracket)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Text.Encoding qualified as T
import Heos.Command (Command (..))
import Heos.Player (PlayerId (..))
import Data.Map.Strict qualified as Map
import Heos.Connection.Internal (Connection (..), findBox, runConnection, sendCommand, splitLines, subscribe)
import Heos.Envelope (Envelope (..), HeosEvent (..), HeosHeader (..), HeosResult (..))
import Heos.Url (decodeQuery)
import Network.Socket (Family (AF_UNIX), Socket, SocketType (Stream), close, socketPair)
import Network.Socket.ByteString (recv, sendAll)
import System.Timeout (timeout)
import Test.Hspec
import Test.QuickCheck (Gen, choose, elements, forAll, listOf, (===))

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

getPlayersLine :: ByteString
getPlayersLine =
  "{\"heos\": {\"command\": \"player/get_players\", \"result\": \"success\", \"message\": \"\"}, \"payload\": [{\"name\": \"Marantz MODEL 60n\", \"pid\": -1245882071}]}\r\n"

heartBeatLine :: ByteString
heartBeatLine =
  "{\"heos\": {\"command\": \"system/heart_beat\", \"result\": \"success\", \"message\": \"\"}}\r\n"

volumeReply :: Int -> ByteString
volumeReply n =
  "{\"heos\": {\"command\": \"player/get_volume\", \"result\": \"success\", \"message\": \"pid=1&level=10&SEQUENCE=" <> BC.pack (show n) <> "\"}}\r\n"

failedLine :: ByteString
failedLine =
  "{\"heos\": {\"command\": \"player/get_players\", \"result\": \"fail\", \"message\": \"eid=2&text=ID Not Valid\"}}\r\n"

-- Send a command and, from the device's side of the socket pair, answer it:
-- wait for the command line, hand it to the check, then write the replies.
-- Returns the reply sendCommand came back with. Bounded, so a lost reply
-- fails the test instead of hanging it.
exchange :: Connection -> Socket -> Command -> (ByteString -> IO ()) -> [ByteString] -> IO Envelope
exchange conn theirs cmd check replies = do
  (reply, ()) <-
    concurrently
      (timeout 1000000 (sendCommand conn cmd) >>= maybe (fail "no reply within a second") pure)
      (recv theirs 4096 >>= check >> mapM_ (sendAll theirs) replies)
  pure reply

-- How many commands are in flight right now.
inFlight :: Connection -> IO Int
inFlight conn = Map.size <$> readTVarIO conn.waiting

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

  describe "sendCommand" $ do
    it "sends the rendered command with its SEQUENCE and returns the reply" $
      withSocketPair $ \(ours, theirs) ->
        runConnection ours $ \conn -> do
          reply <-
            exchange conn theirs GetPlayers
              (\sent -> do
                 sent `shouldSatisfy` BS.isPrefixOf "heos://player/get_players?"
                 sent `shouldSatisfy` BS.isInfixOf "SEQUENCE=0"
                 sent `shouldSatisfy` BS.isSuffixOf "\r\n")
              [getPlayersLine]
          reply.heos.command `shouldBe` "player/get_players"
          reply.heos.result `shouldBe` Just Success

    -- The trap from stage 7: "command under process" is not the reply.
    it "waits through a 'command under process' notice for the real reply" $
      withSocketPair $ \(ours, theirs) ->
        runConnection ours $ \conn -> do
          reply <- exchange conn theirs GetPlayers (const (pure ())) [pendingLine, getPlayersLine]
          reply.heos.command `shouldBe` "player/get_players"
          reply.heos.message `shouldBe` Just ""

    -- A failed result is still the reply to this command; judging it is the
    -- caller's job.
    it "returns a failed reply as the reply, not as an error" $
      withSocketPair $ \(ours, theirs) ->
        runConnection ours $ \conn -> do
          reply <- exchange conn theirs GetPlayers (const (pure ())) [failedLine]
          reply.heos.result `shouldBe` Just Fail
          reply.heos.message `shouldBe` Just "eid=2&text=ID Not Valid"

    it "nothing is in flight before a command and after its reply" $
      withSocketPair $ \(ours, theirs) ->
        runConnection ours $ \conn -> do
          inFlight conn `shouldReturn` 0
          _ <- exchange conn theirs HeartBeat (const (pure ())) [heartBeatLine]
          inFlight conn `shouldReturn` 0

    it "numbers commands from one counter, in order" $
      withSocketPair $ \(ours, theirs) ->
        runConnection ours $ \conn -> do
          _ <- exchange conn theirs HeartBeat (\sent -> sent `shouldSatisfy` BS.isInfixOf "SEQUENCE=0") [heartBeatLine]
          _ <- exchange conn theirs HeartBeat (\sent -> sent `shouldSatisfy` BS.isInfixOf "SEQUENCE=1") [heartBeatLine]
          _ <- exchange conn theirs GetPlayers (\sent -> sent `shouldSatisfy` BS.isInfixOf "SEQUENCE=2") [getPlayersLine]
          pure ()

    -- A reply whose SEQUENCE nobody waits for (late, after a timeout) must
    -- not be handed to whoever waits next.
    it "drops a reply with a SEQUENCE nobody is waiting for" $
      withSocketPair $ \(ours, theirs) ->
        runConnection ours $ \conn -> do
          let late = "{\"heos\": {\"command\": \"player/get_volume\", \"result\": \"success\", \"message\": \"pid=1&level=10&SEQUENCE=99\"}}\r\n"
          answer <- exchange conn theirs (GetVolume (PlayerId 1)) (const (pure ())) [late, volumeReply 0]
          answer.heos.message `shouldBe` Just "pid=1&level=10&SEQUENCE=0"

    -- The lock serialises commands, so two concurrent sendCommand calls
    -- still reach the device one at a time, and each gets its own reply.
    it "serialises concurrent commands and gives each its own reply" $
      withSocketPair $ \(ours, theirs) ->
        runConnection ours $ \conn -> do
          let bounded cmd = timeout 1000000 (sendCommand conn cmd) >>= maybe (fail "no reply within a second") pure
              device = do
                first' <- recv theirs 4096
                sendAll theirs (if "heart_beat" `BS.isInfixOf` first' then heartBeatLine else getPlayersLine)
                second <- recv theirs 4096
                sendAll theirs (if "heart_beat" `BS.isInfixOf` second then heartBeatLine else getPlayersLine)
          ((a, b), ()) <- concurrently (concurrently (bounded HeartBeat) (bounded GetPlayers)) device
          a.heos.command `shouldBe` "system/heart_beat"
          b.heos.command `shouldBe` "player/get_players"
          inFlight conn `shouldReturn` 0

    -- A reply nobody asked for must not be kept for the next command.
    -- The pause lets the reader see the stray while nobody waits.
    it "drops a stray reply instead of handing it to the next command" $
      withSocketPair $ \(ours, theirs) ->
        runConnection ours $ \conn -> do
          sendAll theirs getPlayersLine
          threadDelay 100000
          reply <- exchange conn theirs HeartBeat (const (pure ())) [heartBeatLine]
          reply.heos.command `shouldBe` "system/heart_beat"

    it "still delivers events that arrive while a command is waiting" $
      withSocketPair $ \(ours, theirs) ->
        runConnection ours $ \conn -> do
          ch <- subscribe conn
          reply <-
            exchange conn theirs GetPlayers (const (pure ()))
              [eventLine "player_volume_changed" "pid=1&level=3", getPlayersLine]
          reply.heos.command `shouldBe` "player/get_players"
          nextEvent ch `shouldReturn` volumeEvent "3"

    it "serves two commands one after another" $
      withSocketPair $ \(ours, theirs) ->
        runConnection ours $ \conn -> do
          first' <- exchange conn theirs GetPlayers (const (pure ())) [getPlayersLine]
          second <- exchange conn theirs HeartBeat (const (pure ())) [heartBeatLine]
          first'.heos.command `shouldBe` "player/get_players"
          second.heos.command `shouldBe` "system/heart_beat"

  describe "findBox" $ do
    let boxes = Map.fromList [(7, 'a'), (8, 'b')]

    it "picks the box with the reply's SEQUENCE" $ do
      findBox (Just "pid=1&level=3&SEQUENCE=8") boxes `shouldBe` Just 'b'
      findBox (Just "SEQUENCE=7&pid=1") boxes `shouldBe` Just 'a'

    it "finds nothing for a SEQUENCE nobody is waiting for" $
      findBox (Just "pid=1&SEQUENCE=9") boxes `shouldBe` Nothing

    -- No SEQUENCE: the reply can only be for the single command in flight.
    it "without a SEQUENCE picks the one box in flight" $
      findBox (Just "") (Map.singleton 3 'c') `shouldBe` Just 'c'

    it "without a SEQUENCE and no message still picks the one box in flight" $
      findBox Nothing (Map.singleton 3 'c') `shouldBe` Just 'c'

    -- Guessing between several would hand a reply to the wrong command.
    it "without a SEQUENCE and several in flight refuses to guess" $
      findBox (Just "") boxes `shouldBe` Nothing

    it "without a SEQUENCE and nothing in flight finds nothing" $
      findBox (Just "") (Map.empty :: Map.Map Int Char) `shouldBe` Nothing

    -- A SEQUENCE that isn't a number is as good as none.
    it "treats a non-numeric SEQUENCE as absent" $
      findBox (Just "pid=1&SEQUENCE=abc") (Map.singleton 3 'c') `shouldBe` Just 'c'

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
