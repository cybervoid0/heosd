module Heos.ConnectionSpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (concurrently, withAsync)
import Control.Concurrent.STM (TChan, TVar, atomically, check, modifyTVar', newEmptyTMVarIO, newTVarIO, readTChan, readTVar, readTVarIO)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forever)
import Control.Retry (constantDelay)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Text.Encoding qualified as T
import Heos.Command.Heos (Command (..))
import Heos.Connection.Framing (Pending (..), emptyPending, feedChunk, splitLines)
import Heos.Connection.Heos (findBox, readPid, runConnection, sendCommand, subscribe)
import Heos.Connection.Types (ConnectionError (..), ConnectionProps (..), ConnectionSockets (..), HeosConnection (..), HeosConnectionParams (..), Waiting (..))
import Heos.Envelope.Heos (HeosError (..), HeosEvent (..))
import Heos.FakeDevice (answering, answeringWith, commandPath, failReply, fixture, messageReply, scripted)
import Heos.Time (Seconds (..))
import Heos.Types
import Heos.Url (decodeQuery)
import Network.Socket (Family (AF_UNIX), HostAddress, Socket, SocketType (Stream), close, socketPair, tupleToHostAddress)
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

-- Feed chunks one by one through the reader's framing, the way recv hands
-- them out. Returns the lines and what is still pending, joined back.
feed :: [ByteString] -> ([ByteString], ByteString)
feed = fmap pendingBytes . foldl step ([], emptyPending)
 where
  step (done, tail') chunk =
    let (complete, tail'') = feedChunk tail' chunk
     in (done <> complete, tail'')

pendingBytes :: Pending -> ByteString
pendingBytes tail' = BS.concat (reverse tail'.chunks)

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

-- The recorded player, whole: GetPlayers decodes its reply into [Player].
getPlayersLine :: ByteString
getPlayersLine =
  "{\"heos\": {\"command\": \"player/get_players\", \"result\": \"success\", \"message\": \"\"}, \"payload\": [{\"name\": \"Marantz MODEL 60n\", \"pid\": -1245882071, \"model\": \"Marantz MODEL 60n\", \"version\": \"3.139.173\", \"ip\": \"192.168.1.241\", \"network\": \"wifi\", \"lineout\": 0, \"serial\": \"MCQJ072400069\"}]}\r\n"

-- The pid in getPlayersLine.
model60n :: PlayerId
model60n = PlayerId (-1245882071)

noPlayersLine :: ByteString
noPlayersLine =
  "{\"heos\": {\"command\": \"player/get_players\", \"result\": \"success\", \"message\": \"\"}, \"payload\": []}\r\n"

heartBeatLine :: ByteString
heartBeatLine =
  "{\"heos\": {\"command\": \"system/heart_beat\", \"result\": \"success\", \"message\": \"\"}}\r\n"

volumeReply :: Int -> ByteString
volumeReply n =
  "{\"heos\": {\"command\": \"player/get_volume\", \"result\": \"success\", \"message\": \"pid=1&level=10&SEQUENCE=" <> BC.pack (show n) <> "\"}}\r\n"

failedLine :: ByteString
failedLine =
  "{\"heos\": {\"command\": \"player/get_players\", \"result\": \"fail\", \"message\": \"eid=2&text=ID Not Valid\"}}\r\n"

-- A fail reply to a command without a result: nothing to decode, so the
-- only way to notice it is the result field itself.
failedBeatLine :: ByteString
failedBeatLine =
  "{\"heos\": {\"command\": \"system/heart_beat\", \"result\": \"fail\", \"message\": \"eid=12&text=System error&syserrno=-1001\"}}\r\n"

-- A fail reply with no message at all: still a failure, just a mute one.
failedMuteLine :: ByteString
failedMuteLine =
  "{\"heos\": {\"command\": \"system/heart_beat\", \"result\": \"fail\"}}\r\n"

-- Send a command and, from the device's side of the socket pair, answer it:
-- wait for the command line, hand it to the check, then write the replies.
-- Returns what sendCommand came back with: the reply, decoded for the
-- command. Bounded, so a lost reply fails the test instead of hanging it.
exchange :: HeosConnection -> Socket -> Command a -> (ByteString -> IO ()) -> [ByteString] -> IO a
exchange conn theirs cmd inspect replies = do
  (reply, ()) <-
    concurrently
      (timeout 1000000 (sendCommand conn cmd) >>= maybe (fail "no reply within a second") pure)
      (recv theirs 4096 >>= inspect >> mapM_ (sendAll theirs) replies)
  pure reply

-- A one-second command timeout: long enough for a fake device that answers
-- at once, short enough to fail a test rather than hang it. Heartbeats are
-- an hour apart so they never interfere with the command under test.
testParams :: HeosConnectionParams
testParams = HeosConnectionParams{commandTimeout = Seconds 1, heartbeatDelay = Seconds 3600, maxLineLength = 64 * 1024, port = 1255, retryPolicy = constantDelay 10000}

-- Sockets here come from socketPair, so the address is never dialled;
-- it only fills the field.
loopback :: HostAddress
loopback = tupleToHostAddress (127, 0, 0, 1)

-- The device's ends of both lanes: the test reads here what the daemon
-- sent and writes what the device would answer.
data Device = Device {devEvents :: Socket, devCommands :: Socket}

-- Two socket pairs, one per lane, our ends packed the way runConnection
-- wants them.
withLanes :: (ConnectionSockets -> Device -> IO a) -> IO a
withLanes k =
  withSocketPair $ \(ourEvents, devEvents) ->
    withSocketPair $ \(ourCommands, devCommands) ->
      k (ConnectionSockets loopback ourEvents ourCommands) Device{devEvents, devCommands}

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

-- runConnection hands out the shared cells; the tests wrap them into a
-- connection the way withConnection does, with pid 1 set by hand, since
-- no player is asked for here.
runConn :: HeosConnectionParams -> ConnectionSockets -> (HeosConnection -> IO a) -> IO a
runConn params sockets act =
  runConnection params sockets $ \props -> act HeosConnection{pid = PlayerId 1, sockets, props}

-- Most tests exercise one lane: `ours` becomes the commands socket and the
-- events lane stays idle (open, silent). The same reader serves both
-- sockets, so what these tests establish for one lane holds for the other.
runWith :: HeosConnectionParams -> Socket -> (HeosConnection -> IO a) -> IO a
runWith params ours act =
  withSocketPair $ \(idle, _) -> runConn params (ConnectionSockets loopback idle ours) act

run :: Socket -> (HeosConnection -> IO a) -> IO a
run = runWith testParams

-- How many commands are in flight right now.
inFlight :: HeosConnection -> IO Int
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

    -- The delimiter itself can be cut in two: "\r" at the end of one recv,
    -- "\n" at the start of the next. The reader looks for it across the
    -- boundary, not only inside the new chunk.
    it "reassembles an event whose line end is split between two pieces" $
      withSocketPair $ \(ours, theirs) ->
        run ours $ \conn -> do
          ch <- subscribe conn
          let line = eventLine "player_volume_changed" "pid=1&level=8"
              (firstHalf, secondHalf) = BS.splitAt (BS.length line - 1) line
          BS.last firstHalf `shouldBe` 13 -- "\r"
          sendAll theirs firstHalf
          threadDelay 50000
          sendAll theirs secondHalf
          nextEvent ch `shouldReturn` volumeEvent "8"

    -- A browse reply is far longer than one recv (4 KiB). The reader
    -- accumulates chunks until the line ends and delivers it whole.
    it "delivers a line many times longer than one recv" $
      withSocketPair $ \(ours, theirs) ->
        run ours $ \conn -> do
          ch <- subscribe conn
          let msg = "pid=1&level=9&pad=" <> BC.replicate 20000 'x'
          sendAll theirs (eventLine "player_volume_changed" msg)
          nextEvent ch `shouldReturn` HeosEvent "player_volume_changed" (decodeQuery (T.decodeUtf8 msg))

    -- Without a cap, a device that never sends "\r\n" would grow the
    -- reader's buffer forever. Past maxLineLength the link counts as broken.
    it "ends with LineTooLong when the device sends more than maxLineLength without a line end" $
      withSocketPair $ \(ours, theirs) -> do
        let stuck = runWith testParams{maxLineLength = 1000} ours $ \(_ :: HeosConnection) -> do
              sendAll theirs (BC.replicate 5000 'x')
              forever (threadDelay 100000)
        timeout 2000000 stuck `shouldThrow` (== LineTooLong)

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
        let stuck = run ours $ \(_ :: HeosConnection) -> do
              close theirs
              forever (threadDelay 100000)
        timeout 2000000 stuck `shouldThrow` (== ConnectionLost)

  describe "sendCommand" $ do
    it "sends the rendered command with its SEQUENCE and returns the decoded reply" $
      withSocketPair $ \(ours, theirs) ->
        run ours $ \conn -> do
          reply <-
            exchange
              conn
              theirs
              GetPlayers
              ( \sent -> do
                  sent `shouldSatisfy` BS.isPrefixOf "heos://player/get_players?"
                  sent `shouldSatisfy` BS.isInfixOf "SEQUENCE=0"
                  sent `shouldSatisfy` BS.isSuffixOf "\r\n"
              )
              [getPlayersLine]
          map (.pid) reply `shouldBe` [model60n]

    -- The trap from stage 7: "command under process" is not the reply.
    it "waits through a 'command under process' notice for the real reply" $
      withSocketPair $ \(ours, theirs) ->
        run ours $ \conn -> do
          reply <- exchange conn theirs GetPlayers (const (pure ())) [pendingLine, getPlayersLine]
          map (.pid) reply `shouldBe` [model60n]

    -- A fail reply is checked before any decoding, so the device's own
    -- eid and text come through, whatever the command expected back.
    it "a failed reply carries the device's eid and text" $
      withSocketPair $ \(ours, theirs) ->
        run ours $ \conn ->
          exchange conn theirs GetPlayers (const (pure ())) [failedLine]
            `shouldThrow` (== HeosError{eid = Just 2, text = Just "ID Not Valid"})

    -- The case that used to be swallowed: nothing to decode, so the fail
    -- would have come back as a plain ().
    it "a failed reply to a command without a result is an error, not ()" $
      withSocketPair $ \(ours, theirs) ->
        run ours $ \conn ->
          exchange conn theirs HeartBeat (const (pure ())) [failedBeatLine]
            `shouldThrow` (== HeosError{eid = Just 12, text = Just "System error"})

    it "a failed reply without a message is still a failure" $
      withSocketPair $ \(ours, theirs) ->
        run ours $ \conn ->
          exchange conn theirs HeartBeat (const (pure ())) [failedMuteLine]
            `shouldThrow` (== HeosError{eid = Nothing, text = Nothing})

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
          let late = "{\"heos\": {\"command\": \"player/get_volume\", \"result\": \"success\", \"message\": \"pid=1&level=99&SEQUENCE=99\"}}\r\n"
          answer <- exchange conn theirs (GetVolume (PlayerId 1)) (const (pure ())) [late, volumeReply 0]
          answer `shouldBe` 10

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
          map (.pid) answer `shouldBe` [model60n]

    -- The lock serialises commands, so two concurrent sendCommand calls
    -- still reach the device one at a time, and each gets its own reply.
    it "serialises concurrent commands and gives each its own reply" $
      withSocketPair $ \(ours, theirs) ->
        run ours $ \conn -> do
          -- GADTs switch on MonoLocalBinds: a local that closes over conn is
          -- not generalised, so the signature keeps it usable at two types
          let bounded :: Command a -> IO a
              bounded cmd = timeout 1000000 (sendCommand conn cmd) >>= maybe (fail "no reply within a second") pure
              device = do
                first' <- recv theirs 4096
                sendAll theirs (if "heart_beat" `BS.isInfixOf` first' then heartBeatLine else getPlayersLine)
                second <- recv theirs 4096
                sendAll theirs (if "heart_beat" `BS.isInfixOf` second then heartBeatLine else getPlayersLine)
          ((a, b), ()) <- concurrently (concurrently (bounded HeartBeat) (bounded GetPlayers)) device
          a `shouldBe` ()
          map (.pid) b `shouldBe` [model60n]
          inFlight conn `shouldReturn` 0

    -- A reply nobody asked for must not be kept for the next command.
    -- The pause lets the reader see the stray while nobody waits.
    it "drops a stray reply instead of handing it to the next command" $
      withSocketPair $ \(ours, theirs) ->
        run ours $ \conn -> do
          sendAll theirs getPlayersLine
          threadDelay 100000
          reply <- exchange conn theirs (GetVolume (PlayerId 1)) (const (pure ())) [volumeReply 0]
          reply `shouldBe` 10

    it "still delivers events that arrive while a command is waiting" $
      withSocketPair $ \(ours, theirs) ->
        run ours $ \conn -> do
          ch <- subscribe conn
          reply <-
            exchange
              conn
              theirs
              GetPlayers
              (const (pure ()))
              [eventLine "player_volume_changed" "pid=1&level=3", getPlayersLine]
          map (.pid) reply `shouldBe` [model60n]
          nextEvent ch `shouldReturn` volumeEvent "3"

    it "serves two commands one after another" $
      withSocketPair $ \(ours, theirs) ->
        run ours $ \conn -> do
          first' <- exchange conn theirs GetPlayers (const (pure ())) [getPlayersLine]
          second <- exchange conn theirs HeartBeat (const (pure ())) [heartBeatLine]
          map (.pid) first' `shouldBe` [model60n]
          second `shouldBe` ()

  -- Stage 20. The waiters here run in threads of their own, outside the
  -- race inside runConnection, the way heartbeat and MCP handlers will:
  -- nobody cancels them, so the only way they learn the link is gone is
  -- the Left the reader puts into their boxes on its way out.
  describe "when the connection dies" $ do
    it "a command waiting for its reply fails with ConnectionLost, not a hang" $
      withSocketPair $ \(ours, theirs) -> do
        result <- newEmptyMVar
        connVar <- newEmptyMVar
        outcome <- timeout 2000000 $ try @SomeException $ run ours $ \conn -> do
          putMVar connVar conn
          _ <- forkIO (try @ConnectionError (sendCommand conn HeartBeat) >>= putMVar result)
          threadDelay 100000 -- let it register its box and send
          close theirs -- the device goes away
          forever (threadDelay 100000) :: IO ()
        outcome `shouldSatisfy` \o -> case o of Just (Left _) -> True; _ -> False
        timeout 1000000 (takeMVar result) `shouldReturn` Just (Left ConnectionLost)
        conn <- takeMVar connVar
        inFlight conn `shouldReturn` 0

    it "a command still waiting when the action ends fails with ConnectionLost" $
      withSocketPair $ \(ours, _) -> do
        result <- newEmptyMVar
        run ours $ \conn -> do
          _ <- forkIO (try @ConnectionError (sendCommand conn HeartBeat) >>= putMVar result)
          threadDelay 100000
        -- runConnection returned: the reader was cancelled and failed the waiter
        timeout 1000000 (takeMVar result) `shouldReturn` Just (Left ConnectionLost)

    -- The sender is forked first and sends only once the reader is gone:
    -- closing the socket ends runConnection at once, so nothing can be
    -- forked after that from inside the action.
    it "a command sent after the device went away fails with ConnectionLost" $
      withSocketPair $ \(ours, theirs) -> do
        result <- newEmptyMVar
        _ <- timeout 2000000 $ try @SomeException $ run ours $ \conn -> do
          _ <- forkIO (threadDelay 100000 >> try @ConnectionError (sendCommand conn HeartBeat) >>= putMVar result)
          close theirs
          forever (threadDelay 100000) :: IO ()
        timeout 1000000 (takeMVar result) `shouldReturn` Just (Left ConnectionLost)

  -- Stage 21. A 100 ms timeout here; the daemon uses 5 s.
  describe "when the device is silent" $ do
    it "gives up with ConnectionTimeout and leaves nothing in flight" $
      withSocketPair $ \(ours, _) ->
        runWith testParams{commandTimeout = Seconds 0.1} ours $ \conn -> do
          outcome <- timeout 2000000 (try @ConnectionError (sendCommand conn HeartBeat))
          outcome `shouldBe` Just (Left ConnectionTimeout)
          inFlight conn `shouldReturn` 0

    -- The reply to the abandoned command turns up late: nobody waits for it
    -- any more, so it is dropped, and the next command gets its own reply.
    it "drops the late reply and serves the next command" $
      withSocketPair $ \(ours, theirs) ->
        runWith testParams{commandTimeout = Seconds 0.1} ours $ \conn -> do
          _ <- try @ConnectionError (sendCommand conn HeartBeat) -- times out
          _ <- recv theirs 4096 -- the device finally reads the abandoned command...
          sendAll theirs heartBeatLine -- ...and answers it, too late
          threadDelay 50000
          answer <- exchange conn theirs GetPlayers (const (pure ())) [getPlayersLine]
          map (.pid) answer `shouldBe` [model60n]

    -- The realistic order: the late reply turns up after the next command
    -- has registered its box. Neither reply carries a SEQUENCE (heart_beat
    -- has no arguments, and the device composes "level=" itself, NOTES), so
    -- "the only box in flight" is not enough to tell them apart: only the
    -- command the reply names is. Today the heart_beat reply lands in the
    -- get_volume box and the caller gets a ProtocolError for a reply that
    -- was never meant for it.
    it "does not hand a late reply to the next command just because it is the only one in flight" $
      withSocketPair $ \(ours, theirs) ->
        runWith testParams{commandTimeout = Seconds 0.1} ours $ \conn -> do
          _ <- try @ConnectionError (sendCommand conn HeartBeat) -- times out
          _ <- recv theirs 4096 -- the device reads the abandoned heart_beat
          -- the device reads get_volume, so the box is registered by now, and
          -- only then answers: the late beat first, the real reply after
          level <- exchange conn theirs (GetVolume (PlayerId 1)) (const (pure ())) [heartBeatLine, messageReply "player/get_volume" "pid=1&level=10"]
          level `shouldBe` 10

    -- Worse when the next command returns (): the late success passes for
    -- its reply, nothing is decoded, nobody notices, and the device's real
    -- answer, a refusal, goes stray.
    it "does not let a late success stand in for the reply to a command the device then refuses" $
      withSocketPair $ \(ours, theirs) ->
        runWith testParams{commandTimeout = Seconds 0.1} ours $ \conn -> do
          _ <- try @ConnectionError (sendCommand conn HeartBeat) -- times out
          _ <- recv theirs 4096 -- the device reads the abandoned heart_beat
          exchange conn theirs (SetVolume (PlayerId 1) 20) (const (pure ())) [heartBeatLine, failReply "player/set_volume" "eid=9&text=Out of range"]
            `shouldThrow` (== HeosError{eid = Just 9, text = Just "Out of range"})

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

    -- A refused heartbeat (result=fail) is a dead link as much as a silent
    -- one. Nobody outside asked for the beat, so the device's reply is of
    -- no use to anyone: what comes out of the connection must say "the
    -- link is gone", in the one type 'withReconnect' acts on. Today the
    -- HeosError itself escapes; and with both lanes refusing, which
    -- exception escapes depends on which heartbeat went first: the other
    -- one, cancelled mid-flight, gets ConnectionLost from the readers'
    -- clean-up and may win the race. The error of a background thread
    -- should be wrapped once, at the place it is collected, so that the
    -- outside never sees this lottery.
    it "ends the connection with a ConnectionError, not the device's reply, when a heartbeat is refused" $
      withLanes $ \sockets dev -> do
        let params = testParams{heartbeatDelay = Seconds 0.05}
            refusing = answeringWith (scripted [("system/heart_beat", failedBeatLine)]) (const (pure ()))
        withAsync (refusing dev.devEvents) $ \_ ->
          withAsync (refusing dev.devCommands) $ \_ -> do
            outcome <- timeout 2000000 $ try @ConnectionError $ runConnection params sockets $ \_ ->
              forever (threadDelay 100000) :: IO ()
            outcome `shouldSatisfy` \o -> case o of Just (Left _) -> True; _ -> False

  describe "readPid" $ do
    -- The player is not a parameter: it is whatever the device lists
    -- first in get_players.
    it "takes the pid of the first player the device lists" $
      withLanes $ \sockets dev -> do
        players <- fixture "get_players.json"
        withAsync (answeringWith (scripted [("player/get_players", players)]) (const (pure ())) dev.devCommands) $ \_ -> do
          pid <- runConnection testParams sockets $ \props -> readPid sockets.commands props
          pid `shouldBe` PlayerId (-1245882071)

    -- No players yet (the device is still coming up): a ConnectionError,
    -- so the reconnect loop tries again.
    it "ends with PlayerNotFound when the list is empty" $
      withLanes $ \sockets dev ->
        withAsync (answeringWith (scripted [("player/get_players", noPlayersLine)]) (const (pure ())) dev.devCommands) $ \_ ->
          runConnection testParams sockets (\props -> readPid sockets.commands props)
            `shouldThrow` (== PlayerNotFound)

    -- A start-up command that gets no reply means the connection is no good:
    -- it throws rather than carrying on without a player, and the reconnect
    -- loop takes it from there.
    it "ends with ConnectionTimeout when the device doesn't answer" $
      withLanes $ \sockets _ ->
        runConnection testParams{commandTimeout = Seconds 0.1} sockets (\props -> readPid sockets.commands props)
          `shouldThrow` (== ConnectionTimeout)

  describe "two sockets" $ do
    it "sends commands over the commands socket only" $
      withLanes $ \sockets dev ->
        runConn testParams sockets $ \conn -> do
          _ <- exchange conn dev.devCommands (GetVolume (PlayerId 1)) (const (pure ())) [volumeReply 0]
          timeout 100000 (recv dev.devEvents 4096) `shouldReturn` Nothing

    it "delivers events from the events socket to subscribers" $
      withLanes $ \sockets dev ->
        runConn testParams sockets $ \conn -> do
          ch <- subscribe conn
          sendAll dev.devEvents (eventLine "player_volume_changed" "pid=1&level=7")
          nextEvent ch `shouldReturn` volumeEvent "7"

    -- Either socket going away is the end of the connection: the reader
    -- on it throws, and the race takes the rest down.
    it "ends with ConnectionLost when the device closes the events socket" $
      withLanes $ \sockets dev ->
        runConn testParams sockets (\_ -> close dev.devEvents >> forever (threadDelay 100000) :: IO ())
          `shouldThrow` (== ConnectionLost)

  describe "findBox" $ do
    -- Boxes are TMVars, which compare by identity but have no Show: hence
    -- `is` instead of shouldBe.
    let is got want = (got == want) `shouldBe` True
        twoInFlight = do
          a <- newEmptyTMVarIO
          b <- newEmptyTMVarIO
          pure (a, b, Map.fromList [(7, Waiting "player/get_volume" a), (8, Waiting "browse/search" b)])
        oneInFlight path = do
          c <- newEmptyTMVarIO
          pure (c, Map.singleton 3 (Waiting path c))

    it "picks the box with the reply's SEQUENCE" $ do
      (a, b, boxes) <- twoInFlight
      findBox "browse/search" (Just "sid=10&SEQUENCE=8") boxes `is` Just b
      findBox "player/get_volume" (Just "SEQUENCE=7&pid=1") boxes `is` Just a

    it "finds nothing for a SEQUENCE nobody is waiting for" $ do
      (_, _, boxes) <- twoInFlight
      findBox "browse/search" (Just "pid=1&SEQUENCE=9") boxes `is` Nothing

    -- The number matches but the command does not: a stale box under a
    -- reused number, or a device echoing the wrong thing. Not ours.
    it "refuses a SEQUENCE match when the command differs" $ do
      (_, _, boxes) <- twoInFlight
      findBox "player/get_volume" (Just "sid=10&SEQUENCE=8") boxes `is` Nothing

    -- No SEQUENCE: the reply can only be for the single command in flight,
    -- and the command it names must be that one.
    it "without a SEQUENCE picks the one box in flight" $ do
      (c, boxes) <- oneInFlight "system/heart_beat"
      findBox "system/heart_beat" (Just "") boxes `is` Just c

    it "without a SEQUENCE and no message still picks the one box in flight" $ do
      (c, boxes) <- oneInFlight "system/heart_beat"
      findBox "system/heart_beat" Nothing boxes `is` Just c

    -- The review's case: a late reply to a command that already timed out,
    -- arriving while the next command's box is the only one there.
    it "without a SEQUENCE does not take the one box in flight when it waits for another command" $ do
      (_, boxes) <- oneInFlight "player/get_volume"
      findBox "browse/search" (Just "eid=12&text=System error") boxes `is` Nothing

    -- Guessing between several would hand a reply to the wrong command.
    it "without a SEQUENCE and several in flight refuses to guess" $ do
      (_, _, boxes) <- twoInFlight
      findBox "browse/search" (Just "") boxes `is` Nothing

    it "without a SEQUENCE and nothing in flight finds nothing" $
      findBox "system/heart_beat" (Just "") Map.empty `is` Nothing

    -- A SEQUENCE that isn't a number is as good as none.
    it "treats a non-numeric SEQUENCE as absent" $ do
      (c, boxes) <- oneInFlight "player/get_volume"
      findBox "player/get_volume" (Just "pid=1&SEQUENCE=abc") boxes `is` Just c

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

  describe "feedChunk" $ do
    it "keeps a chunk without a line end pending, without copying it" $
      feedChunk emptyPending "abc" `shouldBe` ([], Pending ["abc"] 3)

    it "ignores an empty chunk" $
      feedChunk (Pending ["ab\r"] 3) "" `shouldBe` ([], Pending ["ab\r"] 3)

    it "adds a second chunk in front of the first" $
      feedChunk (Pending ["abc"] 3) "de" `shouldBe` ([], Pending ["de", "abc"] 5)

    it "joins the pending chunks into the line they complete" $
      feedChunk (Pending ["de", "abc"] 5) "f\r\n" `shouldBe` (["abcdef"], emptyPending)

    it "keeps what follows the last line end as the new tail" $
      feedChunk (Pending ["ab"] 2) "c\r\nd\r\nef" `shouldBe` (["abc", "d"], Pending ["ef"] 2)

    -- "\r" at the end of the tail and "\n" at the start of the chunk is a
    -- line end too, though neither chunk contains it on its own.
    it "finds a line end split between the tail and the chunk" $
      feedChunk (Pending ["ab\r"] 3) "\ncd" `shouldBe` (["ab"], Pending ["cd"] 2)

    describe "properties" $ do
      it "counts the pending bytes correctly" $
        forAll genBytes $ \s ->
          forAll (listOf (choose (0, 5))) $ \cuts ->
            let (_, tail') = foldl (\(_, p) c -> feedChunk p c) ([], emptyPending) (chunksAt cuts s)
             in tail'.size === BS.length (pendingBytes tail')

      it "never leaves a line end in the pending tail" $
        forAll genBytes $ \s ->
          forAll (listOf (choose (0, 5))) $ \cuts ->
            not (BS.isInfixOf "\r\n" (snd (feed (chunksAt cuts s))))
 where
  getPlayers =
    "{\"heos\": {\"command\": \"player/get_players\", \"result\": \"success\", \"message\": \"\"}, \"payload\": [{\"name\": \"Marantz MODEL 60n\", \"pid\": -1245882071}]}"
