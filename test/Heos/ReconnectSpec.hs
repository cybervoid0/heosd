module Heos.ReconnectSpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, readMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (async, concurrently_, race_, wait, withAsync)
import Control.Concurrent.STM (atomically, check, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Exception (ErrorCall (..), IOException, bracket, throwIO, try)
import Control.Monad (forever, void, when)
import Control.Retry (constantDelay, exponentialBackoff)
import Data.ByteString (ByteString)
import GHC.Clock (getMonotonicTime)
import Heos.Command.Heos (Command (..))
import Heos.Connection.Avr (AvrConnectionParams (..))
import Heos.Connection.Heos (sendCommand)
import Heos.Connection.Types (HeosConnectionParams (..))
import Heos.Device (Device (..), LiveState (..), reconnecting)
import Heos.Device.Parameters (DeviceParams (..))
import Heos.Error (CommandError (..), DiscoveryError (..))
import Heos.FakeDevice (answeringWith, failReply, fixture, messageReply, playersLine)
import Heos.FakeDevice qualified as Fake
import Heos.Time (Seconds (..))
import Heos.Types (PlayerId (..))
import Network.Socket (
  Family (AF_INET),
  HostAddress,
  PortNumber,
  SockAddr (SockAddrInet),
  Socket,
  SocketOption (ReuseAddr),
  SocketType (Stream),
  accept,
  bind,
  close,
  defaultPort,
  defaultProtocol,
  getSocketName,
  listen,
  setSocketOption,
  socket,
  tupleToHostAddress,
 )
import Network.Socket.ByteString (recv)
import System.IO.Error (eofErrorType, isEOFError, mkIOError)
import System.Timeout (timeout)
import Test.Hspec

-- These tests go through real TCP on the loopback interface, because the
-- thing under test is what happens around 'withConnection': a refused
-- connect, a closed socket, a fresh connection after that. A socket pair
-- can't do any of this.
--
-- Since stage 23 moved into Device, every connection also runs the
-- start-up sequence and opens the telnet line, so the fake device here
-- answers the start-up queries and a fake amplifier accepts telnet.

loopback :: HostAddress
loopback = tupleToHostAddress (127, 0, 0, 1)

-- A listening socket on the given port (0: picked by the OS). Plays the
-- receiver: the test accepts connections on it, answers, closes them,
-- accepts again.
withDeviceOn :: PortNumber -> (PortNumber -> Socket -> IO a) -> IO a
withDeviceOn wanted k = bracket (socket AF_INET Stream defaultProtocol) close $ \srv -> do
  setSocketOption srv ReuseAddr 1
  bind srv (SockAddrInet wanted loopback)
  listen srv 5
  port <- portOf <$> getSocketName srv
  k port srv
 where
  portOf (SockAddrInet port _) = port
  portOf other = error ("not an IPv4 address: " <> show other)

-- The telnet side of the amplifier: accepts every connection and holds it
-- until the daemon hangs up. Nothing is ever asked of it here; it only
-- has to be there, because withAvr connects on every HEOS connection.
withAmp :: (PortNumber -> IO a) -> IO a
withAmp k = withDeviceOn defaultPort $ \port srv ->
  withAsync (forever (accept srv >>= forkIO . hold . fst)) $ \_ -> k port
 where
  hold sock = void (try @IOException (recv sock 1024)) >> close sock

-- A HEOS device on a port picked by the OS, with the amplifier beside it.
-- Hands the parameters to reach both and the listening socket.
withDevice :: (DeviceParams -> Socket -> IO a) -> IO a
withDevice k = withAmp $ \amp -> withDeviceOn defaultPort $ \port srv -> k (paramsFor port amp) srv

-- A port nobody listens on: picked by the OS and released again, so a
-- connect to it is refused at once. (A bound socket that does not listen
-- would not do: macOS drops the connect instead of refusing it.)
freePort :: IO PortNumber
freePort = withDeviceOn defaultPort (\port _ -> pure port)

-- Parameters for a device on these ports. The tests check that the next
-- attempt happens, not how long it waits, hence the 10 ms policy.
paramsFor :: PortNumber -> PortNumber -> DeviceParams
paramsFor port amp = DeviceParams{heos = heosParamsFor port, avr = AvrConnectionParams{port = amp, requestTimeout = Seconds 0.2}}

-- Change the HEOS side of the parameters. (A record update on `heos` is
-- ambiguous: Device has a field of that name too.)
withHeos :: (HeosConnectionParams -> HeosConnectionParams) -> DeviceParams -> DeviceParams
withHeos f DeviceParams{heos, avr} = DeviceParams{heos = f heos, avr}

heosParamsFor :: PortNumber -> HeosConnectionParams
heosParamsFor port =
  HeosConnectionParams
    { commandTimeout = Seconds 1
    , heartbeatDelay = Seconds 3600
    , maxLineLength = 64 * 1024
    , port
    , retryPolicy = constantDelay 10000
    }

-- The daemon opens two sockets per connection. Accept both and hand them
-- to the continuation; both are closed when it returns.
withBoth :: Socket -> (Socket -> Socket -> IO a) -> IO a
withBoth srv k =
  bracket (fst <$> accept srv) close $ \a ->
    bracket (fst <$> accept srv) close $ \b -> k a b

-- Answer every command on both sockets until the daemon hangs up.
answerBoth :: (ByteString -> ByteString) -> Socket -> Socket -> IO ()
answerBoth reply = answerLanes reply reply

-- One reply function per lane: the events socket is the one the daemon
-- opens first, so it is the one accepted first. A lane the daemon tears
-- down with a reply still unread comes back as a reset on this side; that
-- is the daemon's business, not a reason for the device to stop serving
-- the other lane, so each lane swallows its own IOException.
answerLanes :: (ByteString -> ByteString) -> (ByteString -> ByteString) -> Socket -> Socket -> IO ()
answerLanes replyEvents replyCommands a b = answer replyEvents a `concurrently_` answer replyCommands b
 where
  answer reply sock = void (try @IOException (answeringWith reply (const (pure ())) sock))

-- The recorded replies to the start-up queries. initialize runs on every
-- connection before the action does, so every device here must answer
-- them: one player (pid 1), the recorded sources and criteria, a track.
startUp :: IO [(ByteString, ByteString)]
startUp = do
  sources <- fixture "get_music_sources.json"
  criteria <- fixture "search_criteria.json"
  nowPlaying <- fixture "now_playing.json"
  pure
    [ ("player/get_players", playersLine)
    , ("browse/get_music_sources", sources)
    , ("browse/get_search_criteria", criteria)
    , ("player/get_now_playing_media", nowPlaying)
    , ("player/get_volume", messageReply "player/get_volume" "pid=1&level=15")
    , ("player/get_play_state", messageReply "player/get_play_state" "pid=1&state=play")
    ]

-- The device as the tests here want it: the start-up replies, the given
-- way with the rest, anything else echoed. An empty reply is silence:
-- nothing is sent. The table comes first, so it can override start-up.
deviceThat :: [(ByteString, ByteString)] -> IO (ByteString -> ByteString)
deviceThat table = Fake.scripted . (table <>) <$> startUp

-- The well-behaved device.
echoing :: IO (ByteString -> ByteString)
echoing = deviceThat []

-- Accept one connection (both sockets) and answer until the daemon hangs up.
serve :: Socket -> IO ()
serve srv = echoing >>= (`serveWith` srv)

serveWith :: (ByteString -> ByteString) -> Socket -> IO ()
serveWith reply = serveLanes reply reply

serveLanes :: (ByteString -> ByteString) -> (ByteString -> ByteString) -> Socket -> IO ()
serveLanes replyEvents replyCommands srv = withBoth srv (answerLanes replyEvents replyCommands)

-- A reconnect loop that never finds its way out would hang the suite.
bounded :: IO a -> IO a
bounded io = timeout 5000000 io >>= maybe (fail "timed out after 5 s") pure

-- An address source that answers from a script, one answer per call, and
-- keeps answering the last one. Left is a search that found nothing. Also
-- reports how many times it was asked.
scripted :: [Either DiscoveryError HostAddress] -> IO (IO HostAddress, IO Int)
scripted script = do
  calls <- newTVarIO (0 :: Int)
  let next = do
        n <- atomically $ readTVar calls <* modifyTVar' calls (+ 1)
        either throwIO pure (script !! min n (length script - 1))
  pure (next, readTVarIO calls)

-- An action that counts its starts and returns the count. Being started
-- again is what a reconnect looks like from inside the action.
counting :: IO (IO Int, IO Int)
counting = do
  runs <- newTVarIO (0 :: Int)
  pure (atomically $ modifyTVar' runs (+ 1) >> readTVar runs, readTVarIO runs)

spec :: Spec
spec = describe "reconnecting" $ do
  it "connects once and returns what the action returns" $
    withDevice $ \params srv -> do
      (findAddr, asked) <- scripted [Right loopback]
      (start, runs) <- counting
      device <- async (serve srv)
      result <- bounded $ reconnecting params findAddr (\_ -> ("run " <>) . show <$> start)
      result `shouldBe` "run 1"
      runs >>= (`shouldBe` 1)
      asked >>= (`shouldBe` 1)
      wait device

  -- The action gets a device that went through start-up: the catalogue is
  -- filled and the live state is what the device reported.
  it "hands the action an initialized device" $
    withDevice $ \params srv -> do
      device <- async (serve srv)
      volume <- bounded $ reconnecting params (pure loopback) (\dev -> (.volume) <$> readTVarIO dev.state)
      volume `shouldBe` 15
      wait device

  -- The device reboots: it closes the socket, the reader sees an empty recv,
  -- the action is cancelled, and the whole attempt starts over on a new
  -- socket, start-up and action included.
  it "reconnects and restarts the action when the device closes the connection" $
    withDevice $ \params srv -> do
      (start, _) <- counting
      started <- newEmptyMVar
      well <- echoing
      device <- async $ do
        -- answer until the action is under way, then pull the plug on both
        withBoth srv $ \a b -> race_ (answerBoth well a b) (takeMVar started)
        serve srv
      result <- bounded $ reconnecting params (pure loopback) $ \_ -> do
        n <- start
        if n == 1
          then putMVar started () >> forever (threadDelay 1000000)
          else pure n
      result `shouldBe` 2
      wait device

  -- The device is off: connect is refused at once. The daemon keeps trying
  -- and asks for the address before every attempt, so a device that comes
  -- up later (here: once it has been asked for twice) is found.
  it "keeps trying while connect is refused, asking for the address each time" $
    withAmp $ \amp -> do
      port <- freePort
      asked <- newTVarIO (0 :: Int)
      let findAddr = atomically (modifyTVar' asked (+ 1)) >> pure loopback
      (start, runs) <- counting
      device <- async $ do
        atomically $ readTVar asked >>= check . (>= 2)
        withDeviceOn port (const serve)
      result <- bounded $ reconnecting (paramsFor port amp) findAddr (const start)
      result `shouldBe` 1
      runs >>= (`shouldBe` 1)
      readTVarIO asked >>= (`shouldSatisfy` (>= 2))
      wait device

  -- SSDP found nothing: a failed search is as good a reason to wait and
  -- retry as a dropped connection.
  it "keeps trying while the search finds nothing" $
    withDevice $ \params srv -> do
      (findAddr, asked) <- scripted [Left NoResponse, Left NoResponse, Right loopback]
      (start, runs) <- counting
      device <- async (serve srv)
      result <- bounded $ reconnecting params findAddr (const start)
      result `shouldBe` 1
      runs >>= (`shouldBe` 1)
      asked >>= (`shouldBe` 3)
      wait device

  -- Review item 3. The retry policy doubles from 100 ms; four refused
  -- connects at start-up bring the next delay to 1.6 s. A link dropped
  -- after that must not inherit it: the attempt counter belongs to one
  -- connection, and a lost link starts over with a fresh one, so the
  -- second connection is up well within half a second of the drop.
  it "does not carry the start-up backoff into a reconnect" $
    withAmp $ \amp -> do
      port <- freePort
      asked <- newTVarIO (0 :: Int)
      listening <- newEmptyMVar
      -- The fifth address is handed over only once the device listens:
      -- otherwise the fifth connect races the bind, loses, and the test
      -- sits out one more doubled delay.
      let findAddr = do
            n <- atomically (modifyTVar' asked (+ 1) >> readTVar asked)
            when (n >= 5) (readMVar listening)
            pure loopback
          params = withHeos (\h -> h{retryPolicy = exponentialBackoff 100000}) (paramsFor port amp)
      (start, _) <- counting
      started <- newEmptyMVar
      dropped <- newEmptyMVar
      well <- echoing
      device <- async $ do
        -- four refused connects, then the device is up
        atomically $ readTVar asked >>= check . (>= 5)
        withDeviceOn port $ \_ srv -> do
          putMVar listening ()
          withBoth srv $ \a b -> race_ (answerBoth well a b) (takeMVar started)
          getMonotonicTime >>= putMVar dropped
          serve srv
      reconnectedAt <- bounded $ reconnecting params findAddr $ \_ -> do
        n <- start
        if n == 1
          then putMVar started () >> forever (threadDelay 1000000)
          else getMonotonicTime
      droppedAt <- takeMVar dropped
      (reconnectedAt - droppedAt) `shouldSatisfy` (< 0.5)
      wait device

  -- Only the "device is away" exceptions are retried. A bug in the
  -- action is not a reason to reconnect; it surfaces.
  it "lets an exception that isn't about the device through" $
    withDevice $ \params srv -> do
      device <- async (serve srv)
      bounded (reconnecting params (pure loopback) (\_ -> throwIO (ErrorCall "bug in the action")))
        `shouldThrow` (\(ErrorCall msg) -> msg == "bug in the action")
      wait device

  -- Where "about the device" ends. The three tests below draw the line
  -- between a dead link, which is the connection's business and a reason
  -- to reconnect, and a failed command or a failed action, which belong
  -- to whoever issued them. The device here serves connection after
  -- connection, so a loop that should not happen shows up as a count, or
  -- as the 5 s bound, rather than as a hang.

  -- Stdin ran dry, a file was missing: an IOException from the action is
  -- the action's own problem. The link is fine, and reconnecting would only
  -- run into the same error again, forever.
  it "lets an IOException from the action through instead of reconnecting" $
    withDevice $ \params srv -> do
      (start, runs) <- counting
      withAsync (forever (serve srv)) $ \_ -> do
        bounded (reconnecting params (pure loopback) (\_ -> start >> ioError eof))
          `shouldThrow` isEOFError
        runs >>= (`shouldBe` 1)

  -- One command the device is slow to answer is not a dropped link: the
  -- heartbeat still gets through, the sockets are fine. The caller gets
  -- the timeout and decides what to do; tearing down both sockets, the
  -- telnet session and running discovery again over a slow search is not
  -- it. get_mute is the silent one: it is not part of start-up.
  it "lets a command timeout through instead of reconnecting" $
    withDevice $ \params srv -> do
      (start, runs) <- counting
      silentMute <- deviceThat [("player/get_mute", "")]
      let params' = withHeos (\h -> h{commandTimeout = Seconds 0.1}) params
      withAsync (forever (serveWith silentMute srv)) $ \_ -> do
        bounded (reconnecting params' (pure loopback) (\dev -> start >> sendCommand dev.heos (GetMute (PlayerId 1))))
          `shouldThrow` (== NoReply)
        runs >>= (`shouldBe` 1)

  -- The device refuses a heartbeat (result=fail). Nobody asked for that
  -- beat but the connection itself, so the failure is the connection's: a
  -- dead link, and the daemon reconnects.
  --
  -- Only the events lane refuses, and that lane's heartbeat is the first
  -- thread 'together' starts: so it is its Refused that wins the race and
  -- escapes. Were both lanes to refuse, which exception comes out would
  -- depend on scheduling (see ConnectionSpec, "a refused heartbeat"). The
  -- beat waits 200 ms, long enough for start-up to finish first: a beat
  -- refused during start-up is retried by the policy instead, and the
  -- action would then be started on the second connection, not restarted.
  it "reconnects when the device refuses a heartbeat" $
    withDevice $ \params srv -> do
      (start, _) <- counting
      refusingBeat <- deviceThat [("system/heart_beat", failReply "system/heart_beat" "eid=12&text=System error")]
      well <- echoing
      let params' = withHeos (\h -> h{heartbeatDelay = Seconds 0.2}) params
      -- the first connection refuses the beats on its events lane; the
      -- connections after it behave
      withAsync (serveLanes refusingBeat well srv >> forever (serve srv)) $ \_ -> do
        result <- bounded $ reconnecting params' (pure loopback) $ \_ -> do
          n <- start
          if n == 1
            then forever (threadDelay 1000000)
            else pure n
        result `shouldBe` 2
 where
  eof = mkIOError eofErrorType "getLine" Nothing Nothing
