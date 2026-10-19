module Heos.ConnectionSpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (concurrently, withAsync)
import Control.Concurrent.STM (TChan, TVar, atomically, check, modifyTVar', newTVarIO, readTChan, readTVar, readTVarIO)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.Maybe (isNothing)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Text.Encoding qualified as T
import Heos.Command (Command (..))
import Heos.Player (PlayerId (..))
import Data.Map.Strict qualified as Map
import Heos.Connection.Internal (Connection (..), ConnectionError (..), ConnectionProps (..), ConnectionSockets (..), findBox, initialize, runConnection, sendCommand, splitLines, subscribe)
import Heos.Connection.Parameters (ConnectionParams (..))
import Heos.FakeDevice (answering, commandPath, commandQuery)
import Heos.Time (Seconds (..))
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
exchange conn theirs cmd inspect replies = do
  (reply, ()) <-
    concurrently
      (timeout 1000000 (sendCommand conn cmd) >>= maybe (fail "no reply within a second") unwrap)
      (recv theirs 4096 >>= inspect >> mapM_ (sendAll theirs) replies)
  pure reply

-- A one-second command timeout: long enough for a fake device that answers
-- at once, short enough to fail a test rather than hang it. Heartbeats are
-- an hour apart so they never interfere with the command under test.
testParams :: ConnectionParams
testParams = ConnectionParams{commandTimeout = Seconds 1, heartbeatDelay = Seconds 3600, playerId = PlayerId 1}

-- The device's ends of both lanes: the test reads here what the daemon
-- sent and writes what the device would answer.
data Device = Device {devEvents :: Socket, devCommands :: Socket}

-- Two socket pairs, one per lane, our ends packed the way runConnection
-- wants them.
withLanes :: (ConnectionSockets -> Device -> IO a) -> IO a
withLanes k =
  withSocketPair $ \(ourEvents, devEvents) ->
    withSocketPair $ \(ourCommands, devCommands) ->
      k (ConnectionSockets ourEvents ourCommands) Device{devEvents, devCommands}

-- Which lane a command arrived on, for tests that answer on both.
data Lane = EventsLane | CommandsLane deriving (Show, Eq)

-- A device that answers on both lanes and logs every command with its
-- lane, newest first.
withAnsweringDevice :: Device -> (TVar [(Lane, ByteString)] -> IO a) -> IO a
withAnsweringDevice dev k = do
  seen <- newTVarIO []
  let record lane line = atomically $ modifyTVar' seen ((lane, line) :)
  withAsync (answering (record EventsLane) dev.devEvents) $ \_ ->
    withAsync (answering (record CommandsLane) dev.devCommands) $ \_ ->
      k seen

-- Most tests exercise one lane: `ours` becomes the commands socket and the
-- events lane stays idle (open, silent). The same reader serves both
-- sockets, so what these tests establish for one lane holds for the other.
runWith :: ConnectionParams -> Socket -> (Connection -> IO a) -> IO a
runWith params ours act =
  withSocketPair $ \(idle, _) -> runConnection params (ConnectionSockets idle ours) act

run :: Socket -> (Connection -> IO a) -> IO a
run = runWith testParams

-- The tests here expect the device to answer; a Left is a failure.
unwrap :: Either ConnectionError Envelope -> IO Envelope
unwrap = either (\e -> fail ("sendCommand failed: " <> show e)) pure

-- How many commands are in flight right now.
inFlight :: Connection -> IO Int
inFlight conn = Map.size <$> readTVarIO conn.props.waiting

spec :: Spec
spec = do
  describe "runConnection" $ do
    it "returns what the action returns" $
      withSocketPair $ \(ours, _) ->
        run ours (\_ -> pure (42 :: Int)) `shouldReturn` 42

    it "delivers events to a subscriber in the order they arrived" $
      withSocketPair $ \(ours, theirs) ->
        run ours $ \conn -> do
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
        run ours $ \conn -> do
          ch <- subscribe conn
          sendAll theirs responseLine
          sendAll theirs pendingLine
          sendAll theirs (eventLine "player_state_changed" "pid=1&state=play")
          nextEvent ch `shouldReturn` HeosEvent "player_state_changed" (decodeQuery "pid=1&state=play")

    -- recv hands out arbitrary chunks; a line split across two of them is
    -- still one line. Two writes with a pause in between force two recvs.
    it "reassembles an event that arrived in two pieces" $
      withSocketPair $ \(ours, theirs) ->
        run ours $ \conn -> do
          ch <- subscribe conn
          let (firstHalf, secondHalf) = BS.splitAt 30 (eventLine "player_volume_changed" "pid=1&level=7")
          sendAll theirs firstHalf
          threadDelay 50000
          sendAll theirs secondHalf
          nextEvent ch `shouldReturn` volumeEvent "7"

    it "gives every subscriber its own copy of each event" $
      withSocketPair $ \(ours, theirs) ->
        run ours $ \conn -> do
          ch1 <- subscribe conn
          ch2 <- subscribe conn
          sendAll theirs (eventLine "player_volume_changed" "pid=1&level=5")
          nextEvent ch1 `shouldReturn` volumeEvent "5"
          nextEvent ch2 `shouldReturn` volumeEvent "5"

    -- The device closing the connection must end runConnection with
    -- ConnectionLost, not leave the action waiting forever (and not spin the
    -- reader). It is ConnectionLost, not a plain IOException, so that
    -- withReconnect (stage 23) can tell a dropped link from any other error.
    it "fails with ConnectionLost, and stops the action, when the device closes the connection" $
      withSocketPair $ \(ours, theirs) -> do
        let stuck = run ours $ \(_ :: Connection) -> do
              close theirs
              forever (threadDelay 100000)
        timeout 2000000 stuck `shouldThrow` (== ConnectionLost)

  describe "sendCommand" $ do
    it "sends the rendered command with its SEQUENCE and returns the reply" $
      withSocketPair $ \(ours, theirs) ->
        run ours $ \conn -> do
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
        run ours $ \conn -> do
          reply <- exchange conn theirs GetPlayers (const (pure ())) [pendingLine, getPlayersLine]
          reply.heos.command `shouldBe` "player/get_players"
          reply.heos.message `shouldBe` Just ""

    -- A failed result is still the reply to this command; judging it is the
    -- caller's job.
    it "returns a failed reply as the reply, not as an error" $
      withSocketPair $ \(ours, theirs) ->
        run ours $ \conn -> do
          reply <- exchange conn theirs GetPlayers (const (pure ())) [failedLine]
          reply.heos.result `shouldBe` Just Fail
          reply.heos.message `shouldBe` Just "eid=2&text=ID Not Valid"

    it "nothing is in flight before a command and after its reply" $
      withSocketPair $ \(ours, theirs) ->
        run ours $ \conn -> do
          inFlight conn `shouldReturn` 0
          _ <- exchange conn theirs HeartBeat (const (pure ())) [heartBeatLine]
          inFlight conn `shouldReturn` 0

    it "numbers commands from one counter, in order" $
      withSocketPair $ \(ours, theirs) ->
        run ours $ \conn -> do
          _ <- exchange conn theirs HeartBeat (\sent -> sent `shouldSatisfy` BS.isInfixOf "SEQUENCE=0") [heartBeatLine]
          _ <- exchange conn theirs HeartBeat (\sent -> sent `shouldSatisfy` BS.isInfixOf "SEQUENCE=1") [heartBeatLine]
          _ <- exchange conn theirs GetPlayers (\sent -> sent `shouldSatisfy` BS.isInfixOf "SEQUENCE=2") [getPlayersLine]
          pure ()

    -- A reply whose SEQUENCE nobody waits for (late, after a timeout) must
    -- not be handed to whoever waits next.
    it "drops a reply with a SEQUENCE nobody is waiting for" $
      withSocketPair $ \(ours, theirs) ->
        run ours $ \conn -> do
          let late = "{\"heos\": {\"command\": \"player/get_volume\", \"result\": \"success\", \"message\": \"pid=1&level=10&SEQUENCE=99\"}}\r\n"
          answer <- exchange conn theirs (GetVolume (PlayerId 1)) (const (pure ())) [late, volumeReply 0]
          answer.heos.message `shouldBe` Just "pid=1&level=10&SEQUENCE=0"

    -- Stage 19: whoever gives up waiting (a timeout, a cancelled thread)
    -- must leave nothing behind, or a late reply would find a box nobody
    -- reads and the next command would be routed by a map with ghosts in it.
    it "removes its box from the map when the wait is cancelled" $
      withSocketPair $ \(ours, _) ->
        run ours $ \conn -> do
          gaveUp <- timeout 100000 (sendCommand conn HeartBeat)
          gaveUp `shouldSatisfy` isNothing
          inFlight conn `shouldReturn` 0

    it "lets the next command through after a cancelled one" $
      withSocketPair $ \(ours, theirs) ->
        run ours $ \conn -> do
          _ <- timeout 100000 (sendCommand conn HeartBeat)
          _ <- recv theirs 4096 -- the device swallows the abandoned command
          answer <- exchange conn theirs GetPlayers (const (pure ())) [getPlayersLine]
          answer.heos.command `shouldBe` "player/get_players"

    -- The lock serialises commands, so two concurrent sendCommand calls
    -- still reach the device one at a time, and each gets its own reply.
    it "serialises concurrent commands and gives each its own reply" $
      withSocketPair $ \(ours, theirs) ->
        run ours $ \conn -> do
          let bounded cmd = timeout 1000000 (sendCommand conn cmd) >>= maybe (fail "no reply within a second") unwrap
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
        run ours $ \conn -> do
          sendAll theirs getPlayersLine
          threadDelay 100000
          reply <- exchange conn theirs HeartBeat (const (pure ())) [heartBeatLine]
          reply.heos.command `shouldBe` "system/heart_beat"

    it "still delivers events that arrive while a command is waiting" $
      withSocketPair $ \(ours, theirs) ->
        run ours $ \conn -> do
          ch <- subscribe conn
          reply <-
            exchange conn theirs GetPlayers (const (pure ()))
              [eventLine "player_volume_changed" "pid=1&level=3", getPlayersLine]
          reply.heos.command `shouldBe` "player/get_players"
          nextEvent ch `shouldReturn` volumeEvent "3"

    it "serves two commands one after another" $
      withSocketPair $ \(ours, theirs) ->
        run ours $ \conn -> do
          first' <- exchange conn theirs GetPlayers (const (pure ())) [getPlayersLine]
          second <- exchange conn theirs HeartBeat (const (pure ())) [heartBeatLine]
          first'.heos.command `shouldBe` "player/get_players"
          second.heos.command `shouldBe` "system/heart_beat"

  -- Stage 20. The waiters here run in threads of their own, outside the
  -- race inside runConnection, the way heartbeat and MCP handlers will:
  -- nobody cancels them, so the only way they learn the link is gone is
  -- the Left the reader puts into their boxes on its way out.
  describe "when the connection dies" $ do
    it "a command waiting for its reply gets Left ConnectionLost, not a hang" $
      withSocketPair $ \(ours, theirs) -> do
        result <- newEmptyMVar
        connVar <- newEmptyMVar
        outcome <- timeout 2000000 $ try @SomeException $ run ours $ \conn -> do
          putMVar connVar conn
          _ <- forkIO (sendCommand conn HeartBeat >>= putMVar result)
          threadDelay 100000 -- let it register its box and send
          close theirs -- the device goes away
          forever (threadDelay 100000) :: IO ()
        outcome `shouldSatisfy` \o -> case o of Just (Left _) -> True; _ -> False
        timeout 1000000 (takeMVar result) `shouldReturn` Just (Left ConnectionLost)
        conn <- takeMVar connVar
        inFlight conn `shouldReturn` 0

    it "a command still waiting when the action ends gets Left ConnectionLost" $
      withSocketPair $ \(ours, _) -> do
        result <- newEmptyMVar
        run ours $ \conn -> do
          _ <- forkIO (sendCommand conn HeartBeat >>= putMVar result)
          threadDelay 100000
        -- runConnection returned: the reader was cancelled and failed the waiter
        timeout 1000000 (takeMVar result) `shouldReturn` Just (Left ConnectionLost)

    -- The sender is forked first and sends only once the reader is gone:
    -- closing the socket ends runConnection at once, so nothing can be
    -- forked after that from inside the action.
    it "a command sent after the device went away fails with Left ConnectionLost" $
      withSocketPair $ \(ours, theirs) -> do
        result <- newEmptyMVar
        _ <- timeout 2000000 $ try @SomeException $ run ours $ \conn -> do
          _ <- forkIO (threadDelay 100000 >> sendCommand conn HeartBeat >>= putMVar result)
          close theirs
          forever (threadDelay 100000) :: IO ()
        timeout 1000000 (takeMVar result) `shouldReturn` Just (Left ConnectionLost)

  -- Stage 21. A 100 ms timeout here; the daemon uses 5 s.
  describe "when the device is silent" $ do
    it "gives up with Left ConnectionTimeout and leaves nothing in flight" $
      withSocketPair $ \(ours, _) ->
        runWith testParams{commandTimeout = Seconds 0.1} ours $ \conn -> do
          outcome <- timeout 2000000 (sendCommand conn HeartBeat)
          outcome `shouldBe` Just (Left ConnectionTimeout)
          inFlight conn `shouldReturn` 0

    -- The reply to the abandoned command turns up late: nobody waits for it
    -- any more, so it is dropped, and the next command gets its own reply.
    it "drops the late reply and serves the next command" $
      withSocketPair $ \(ours, theirs) ->
        runWith testParams{commandTimeout = Seconds 0.1} ours $ \conn -> do
          _ <- sendCommand conn HeartBeat
          _ <- recv theirs 4096 -- the device finally reads the abandoned command...
          sendAll theirs heartBeatLine -- ...and answers it, too late
          threadDelay 50000
          answer <- exchange conn theirs GetPlayers (const (pure ())) [getPlayersLine]
          answer.heos.command `shouldBe` "player/get_players"

  -- Stage 22. Heartbeats every 50 ms here; the daemon sends one every 30 s.
  describe "heartbeat" $ do
    -- The device drops a socket that stays silent, so each lane gets its
    -- own beats.
    it "sends system/heart_beat on each socket at the configured interval" $
      withLanes $ \sockets dev ->
        withAnsweringDevice dev $ \seen -> do
          let params = testParams{heartbeatDelay = Seconds 0.05}
              beatsOn lane = length . filter ((== lane) . fst)
          -- wait for three beats on each lane, then let the action end
          done <- timeout 2000000 $ runConnection params sockets $ \_ ->
            atomically $ readTVar seen >>= \l -> check (beatsOn EventsLane l >= 3 && beatsOn CommandsLane l >= 3)
          done `shouldBe` Just ()
          readTVarIO seen >>= \l -> map (commandPath . snd) l `shouldSatisfy` all (== "system/heart_beat")

    -- A heartbeat nobody answers is how the daemon learns the link is dead
    -- without anyone sending a command: the connection must end with an
    -- error, not idle on.
    it "ends the connection when a heartbeat gets no reply" $
      withSocketPair $ \(ours, _) -> do
        let params = testParams{heartbeatDelay = Seconds 0.05, commandTimeout = Seconds 0.1}
        outcome <- timeout 2000000 $ try @SomeException $ runWith params ours $ \_ ->
          forever (threadDelay 100000) :: IO ()
        outcome `shouldSatisfy` \o -> case o of Just (Left _) -> True; _ -> False

  describe "initialize" $ do
    -- The order the vendor prescribes: events off first, so the state
    -- queries aren't interleaved with notifications; events on last.
    -- Subscriptions go over the events socket, queries over the commands
    -- socket, and the events socket never carries a query.
    it "runs the start-up sequence in the prescribed order, each command on its lane" $
      withLanes $ \sockets dev ->
        withAnsweringDevice dev $ \seen -> do
          runConnection testParams sockets initialize
          commands <- reverse <$> readTVarIO seen
          map (fmap commandPath) commands
            `shouldBe` [ (EventsLane, "system/register_for_change_events")
                       , (CommandsLane, "player/get_players")
                       , (CommandsLane, "group/get_groups")
                       , (CommandsLane, "browse/get_music_sources")
                       , (CommandsLane, "player/get_now_playing_media")
                       , (CommandsLane, "player/get_volume")
                       , (CommandsLane, "player/get_play_state")
                       , (EventsLane, "system/register_for_change_events")
                       ]
          let enables = [BC.takeWhile (/= '&') q | q <- map (commandQuery . snd) commands, "enable=" `BS.isPrefixOf` q]
          enables `shouldBe` ["enable=off", "enable=on"]

    -- The player queries go to the configured player, not to a hard-coded one.
    it "asks about the player from the parameters" $
      withLanes $ \sockets dev ->
        withAnsweringDevice dev $ \seen -> do
          runConnection testParams{playerId = PlayerId 42} sockets initialize
          commands <- map snd <$> readTVarIO seen
          filter (BS.isPrefixOf "pid=42" . commandQuery) commands `shouldSatisfy` ((== 3) . length)

    -- A start-up command that gets no reply means the connection is no good:
    -- initialize throws rather than carrying on with half a state, and the
    -- reconnect loop takes it from there.
    it "ends with ConnectionTimeout when the device doesn't answer" $
      withSocketPair $ \(ours, _) ->
        runWith testParams{commandTimeout = Seconds 0.1} ours initialize
          `shouldThrow` (== ConnectionTimeout)

  describe "two sockets" $ do
    it "sends commands over the commands socket only" $
      withLanes $ \sockets dev ->
        runConnection testParams sockets $ \conn -> do
          _ <- exchange conn dev.devCommands (GetVolume (PlayerId 1)) (const (pure ())) [volumeReply 0]
          timeout 100000 (recv dev.devEvents 4096) `shouldReturn` Nothing

    it "delivers events from the events socket to subscribers" $
      withLanes $ \sockets dev ->
        runConnection testParams sockets $ \conn -> do
          ch <- subscribe conn
          sendAll dev.devEvents (eventLine "player_volume_changed" "pid=1&level=7")
          nextEvent ch `shouldReturn` volumeEvent "7"

    -- Either socket going away is the end of the connection: the reader
    -- on it throws, and the race takes the rest down.
    it "ends with ConnectionLost when the device closes the events socket" $
      withLanes $ \sockets dev ->
        runConnection testParams sockets (\_ -> close dev.devEvents >> forever (threadDelay 100000) :: IO ())
          `shouldThrow` (== ConnectionLost)

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
