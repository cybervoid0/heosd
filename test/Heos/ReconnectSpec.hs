module Heos.ReconnectSpec (spec) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (async, concurrently_, race_, wait)
import Control.Concurrent.STM (atomically, check, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Exception (ErrorCall (..), bracket, throwIO)
import Control.Monad (forever)
import Control.Retry (constantDelay)
import Heos.Connection.Reconnect (reconnecting)
import Heos.Discovery (DiscoveryError (..))
import Heos.FakeDevice (answeringWith, playersLine)
import Heos.FakeDevice qualified as Fake
import Heos.Connection.Types (HeosConnectionParams (..))
import Heos.Time (Seconds (..))
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
import System.Timeout (timeout)
import Test.Hspec

-- These tests go through real TCP on the loopback interface, because the
-- thing under test is what happens around 'withConnection': a refused
-- connect, a closed socket, a fresh connection after that. A socket pair
-- can't do any of this.

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

withDevice :: (PortNumber -> Socket -> IO a) -> IO a
withDevice = withDeviceOn defaultPort

-- A port nobody listens on: picked by the OS and released again, so a
-- connect to it is refused at once. (A bound socket that does not listen
-- would not do: macOS drops the connect instead of refusing it.)
freePort :: IO PortNumber
freePort = withDeviceOn defaultPort (\port _ -> pure port)

-- Parameters for a device on this port. The tests check that the next
-- attempt happens, not how long it waits, hence the 10 ms policy.
paramsFor :: PortNumber -> HeosConnectionParams
paramsFor port =
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
-- get_players gets a real player: withConnection needs one to come up.
answerBoth :: Socket -> Socket -> IO ()
answerBoth a b = answer a `concurrently_` answer b
 where
  answer = answeringWith (Fake.scripted [("player/get_players", playersLine)]) (const (pure ()))

serve :: Socket -> IO ()
serve srv = withBoth srv answerBoth

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
    withDevice $ \port srv -> do
      (findAddr, asked) <- scripted [Right loopback]
      (start, runs) <- counting
      device <- async (serve srv)
      result <- bounded $ reconnecting (paramsFor port) findAddr (\_ -> ("run " <>) . show <$> start)
      result `shouldBe` "run 1"
      runs >>= (`shouldBe` 1)
      asked >>= (`shouldBe` 1)
      wait device

  -- The device reboots: it closes the socket, the reader sees an empty recv,
  -- the action is cancelled, and the whole attempt starts over on a new
  -- socket, action included.
  it "reconnects and restarts the action when the device closes the connection" $
    withDevice $ \port srv -> do
      (start, _) <- counting
      started <- newEmptyMVar
      device <- async $ do
        -- answer until the action is under way, then pull the plug on both
        withBoth srv $ \a b -> race_ (answerBoth a b) (takeMVar started)
        serve srv
      result <- bounded $ reconnecting (paramsFor port) (pure loopback) $ \_ -> do
        n <- start
        if n == 1
          then putMVar started () >> forever (threadDelay 1000000)
          else pure n
      result `shouldBe` 2
      wait device

  -- The device is off: connect is refused at once. The daemon keeps trying
  -- and asks for the address before every attempt, so a device that comes
  -- up later (here: once it has been asked for twice) is found.
  it "keeps trying while connect is refused, asking for the address each time" $ do
    port <- freePort
    asked <- newTVarIO (0 :: Int)
    let findAddr = atomically (modifyTVar' asked (+ 1)) >> pure loopback
    (start, runs) <- counting
    device <- async $ do
      atomically $ readTVar asked >>= check . (>= 2)
      withDeviceOn port (const serve)
    result <- bounded $ reconnecting (paramsFor port) findAddr (const start)
    result `shouldBe` 1
    runs >>= (`shouldBe` 1)
    readTVarIO asked >>= (`shouldSatisfy` (>= 2))
    wait device

  -- SSDP found nothing: a failed search is as good a reason to wait and
  -- retry as a dropped connection.
  it "keeps trying while the search finds nothing" $
    withDevice $ \port srv -> do
      (findAddr, asked) <- scripted [Left NoResponse, Left NoResponse, Right loopback]
      (start, runs) <- counting
      device <- async (serve srv)
      result <- bounded $ reconnecting (paramsFor port) findAddr (const start)
      result `shouldBe` 1
      runs >>= (`shouldBe` 1)
      asked >>= (`shouldBe` 3)
      wait device

  -- Only the three "device is away" exceptions are retried. A bug in the
  -- action is not a reason to reconnect; it surfaces.
  it "lets an exception that isn't about the device through" $
    withDevice $ \port srv -> do
      device <- async (serve srv)
      bounded (reconnecting (paramsFor port) (pure loopback) (\_ -> throwIO (ErrorCall "bug in the action")))
        `shouldThrow` (\(ErrorCall msg) -> msg == "bug in the action")
      wait device
