module Heos.ReconnectSpec (spec) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (atomically, newTVarIO, readTVarIO, stateTVar)
import Control.Exception (ErrorCall (..), bracket, throwIO)
import Control.Monad (forever)
import Control.Retry (RetryPolicyM, constantDelay)
import Heos.Connection.Internal (reconnecting)
import Heos.Discovery (DiscoveryError (..))
import Network.Socket (
  Family (AF_INET),
  HostAddress,
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

-- A listening socket on a port picked by the OS. Plays the receiver: the
-- test accepts connections on it, closes them, accepts again.
withDevice :: (SockAddr -> Socket -> IO a) -> IO a
withDevice k = bracket (socket AF_INET Stream defaultProtocol) close $ \srv -> do
  setSocketOption srv ReuseAddr 1
  bind srv (SockAddrInet defaultPort loopback)
  listen srv 5
  addr <- getSocketName srv
  k addr srv

-- Nobody listens on port 1: connect is refused at once, no waiting.
deadAddr :: SockAddr
deadAddr = SockAddrInet 1 loopback

-- The tests check that the next attempt happens, not how long it waits.
quick :: RetryPolicyM IO
quick = constantDelay 10000

-- A reconnect loop that never finds its way out would hang the suite.
bounded :: IO a -> IO a
bounded io = timeout 5000000 io >>= maybe (fail "timed out after 5 s") pure

-- An address source that answers from a script, one answer per call, and
-- keeps answering the last one. Left is a search that found nothing. Also
-- reports how many times it was asked.
scripted :: [Either DiscoveryError SockAddr] -> IO (IO SockAddr, IO Int)
scripted script = do
  calls <- newTVarIO (0 :: Int)
  let next = do
        n <- atomically $ stateTVar calls (\n -> (n, n + 1))
        either throwIO pure (script !! min n (length script - 1))
  pure (next, readTVarIO calls)

-- An action that counts its starts and returns the count. Being started
-- again is what a reconnect looks like from inside the action.
counting :: IO (IO Int, IO Int)
counting = do
  runs <- newTVarIO (0 :: Int)
  pure (atomically $ stateTVar runs (\n -> (n + 1, n + 1)), readTVarIO runs)

spec :: Spec
spec = describe "reconnecting" $ do
  it "connects once and returns what the action returns" $
    withDevice $ \addr srv -> do
      (findAddr, asked) <- scripted [Right addr]
      (start, runs) <- counting
      device <- async (fst <$> accept srv)
      result <- bounded $ reconnecting quick findAddr (\_ -> ("run " <>) . show <$> start)
      result `shouldBe` "run 1"
      runs >>= (`shouldBe` 1)
      asked >>= (`shouldBe` 1)
      wait device >>= close

  -- The device reboots: it closes the socket, the reader sees an empty recv,
  -- the action is cancelled, and the whole attempt starts over on a new
  -- socket, action included.
  it "reconnects and restarts the action when the device closes the connection" $
    withDevice $ \addr srv -> do
      (start, _) <- counting
      started <- newEmptyMVar
      device <- async $ do
        (first, _) <- accept srv
        takeMVar started -- let the action get going before pulling the plug
        close first
        fst <$> accept srv
      result <- bounded $ reconnecting quick (pure addr) $ \_ -> do
        n <- start
        if n == 1
          then putMVar started () >> forever (threadDelay 1000000)
          else pure n
      result `shouldBe` 2
      wait device >>= close

  -- The device is off, or the address is stale after a DHCP change: connect
  -- is refused. The next attempt asks for the address again rather than
  -- hammering the old one.
  it "keeps trying while connect is refused, asking for the address each time" $
    withDevice $ \addr srv -> do
      (findAddr, asked) <- scripted [Right deadAddr, Right deadAddr, Right addr]
      (start, runs) <- counting
      device <- async (fst <$> accept srv)
      result <- bounded $ reconnecting quick findAddr (const start)
      result `shouldBe` 1
      runs >>= (`shouldBe` 1)
      asked >>= (`shouldBe` 3)
      wait device >>= close

  -- SSDP found nothing: a failed search is as good a reason to wait and
  -- retry as a dropped connection.
  it "keeps trying while the search finds nothing" $
    withDevice $ \addr srv -> do
      (findAddr, asked) <- scripted [Left NoResponse, Left NoResponse, Right addr]
      (start, runs) <- counting
      device <- async (fst <$> accept srv)
      result <- bounded $ reconnecting quick findAddr (const start)
      result `shouldBe` 1
      runs >>= (`shouldBe` 1)
      asked >>= (`shouldBe` 3)
      wait device >>= close

  -- Only the three "device is away" exceptions are retried. A bug in the
  -- action is not a reason to reconnect; it surfaces.
  it "lets an exception that isn't about the device through" $
    withDevice $ \addr srv -> do
      device <- async (fst <$> accept srv)
      bounded (reconnecting quick (pure addr) (\_ -> throwIO (ErrorCall "bug in the action")))
        `shouldThrow` (\(ErrorCall msg) -> msg == "bug in the action")
      wait device >>= close
