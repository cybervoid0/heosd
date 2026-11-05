module Heos.AvrSpec (spec) where

import Control.Concurrent.Async (concurrently, withAsync, wait)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO, stateTVar, writeTVar)
import Control.Exception (IOException, bracket, try)
import Control.Monad (forM_, unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Either (isLeft)
import Effectful (runEff)
import Heos.Command.Avr (AvrCommand (..), renderAvrCommand)
import Heos.Connection.Avr (AvrConnection, AvrConnectionError (..), AvrConnectionParams (..), answerOf, sendAvrCommand, withAvr)
import Heos.Effect.Avr (getPower, setPower)
import Heos.Envelope.Avr (decodeAvrLine)
import Heos.Handler.AvrLive (runAvrLive)
import Heos.Time (Seconds (..))
import Heos.Types (OnOff (..))
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
import Network.Socket.ByteString (recv, sendAll)
import Test.Hspec

-- The telnet side of the amplifier: ASCII lines ending in "\r", no
-- sequence numbers, and events ("MV45") mixed into the same stream as
-- replies. These tests run a fake amplifier on the loopback interface and
-- talk to it through 'withAvr', so the port comes from the OS and the
-- timeout from the test.

loopback :: HostAddress
loopback = tupleToHostAddress (127, 0, 0, 1)

-- Short enough to fail a test rather than hang it; the device answers at
-- once when it answers at all.
paramsFor :: PortNumber -> AvrConnectionParams
paramsFor port = AvrConnectionParams{port, requestTimeout = Seconds 0.2}

-- What the amplifier does with its end of the socket once the daemon has
-- connected. Gets the log to record what the daemon sent.
type Script = TVar [ByteString] -> Socket -> IO ()

-- Answer each command line with what the function returns; an empty
-- answer is silence. Runs until the daemon hangs up.
replyingWith :: (ByteString -> IO ByteString) -> Script
replyingWith reply seen sock = go ""
 where
  go rest = do
    chunk <- recv sock 1024
    unless (BS.null chunk) $ do
      let pieces = BC.split '\r' (rest <> chunk)
      forM_ (init pieces) $ \line -> do
        atomically $ modifyTVar' seen (line :)
        answer <- reply line
        unless (BS.null answer) $ sendAll sock answer
      go (last pieces)

replying :: (ByteString -> ByteString) -> Script
replying reply = replyingWith (pure . reply)

-- The amplifier as the daemon finds it when the power state is `pw`:
-- answers the question and echoes a change of state. Stateless: the
-- question is always answered with `pw`.
poweredAmp :: OnOff -> ByteString -> ByteString
poweredAmp pw = \case
  "PW?" -> statusLine pw
  "PWON" -> statusLine On
  "PWSTANDBY" -> statusLine Off
  _ -> ""

statusLine :: OnOff -> ByteString
statusLine On = "PWON\r"
statusLine Off = "PWSTANDBY\r"

-- The same amplifier, but it remembers the state it was switched to.
statefulAmp :: OnOff -> IO (ByteString -> IO ByteString)
statefulAmp initial = do
  power <- newTVarIO initial
  pure $ \case
    "PW?" -> statusLine <$> readTVarIO power
    "PWON" -> statusLine On <$ atomically (writeTVar power On)
    "PWSTANDBY" -> statusLine Off <$ atomically (writeTVar power Off)
    _ -> pure ""

-- An amplifier on the loopback interface, port picked by the OS. Runs the
-- script on the accepted socket and the action over 'withAvr' to it.
-- Returns the action's result and the lines the amplifier received.
withAmp :: Script -> (AvrConnection -> IO a) -> IO (a, [ByteString])
withAmp script act = bracket (socket AF_INET Stream defaultProtocol) close $ \srv -> do
  setSocketOption srv ReuseAddr 1
  bind srv (SockAddrInet defaultPort loopback)
  listen srv 1
  port <- portOf <$> getSocketName srv
  seen <- newTVarIO []
  withAsync (bracket (fst <$> accept srv) close (script seen)) $ \amp -> do
    result <- withAvr (paramsFor port) loopback act
    wait amp
    sent <- reverse <$> readTVarIO seen
    pure (result, sent)
 where
  portOf (SockAddrInet port _) = port
  portOf other = error ("not an IPv4 address: " <> show other)

spec :: Spec
spec = do
  describe "renderAvrCommand" $ do
    it "ends every command with a carriage return" $ do
      renderAvrCommand GetPower `shouldBe` "PW?\r"
      renderAvrCommand (SetPower On) `shouldBe` "PWON\r"
      renderAvrCommand (SetPower Off) `shouldBe` "PWSTANDBY\r"

  -- AvrResponse has no Eq, so the decoded line is judged through answerOf.
  describe "answerOf" $ do
    it "takes the power status as the answer to PW?" $ do
      answerOf GetPower (decodeAvrLine "PWON") `shouldBe` Just On
      answerOf GetPower (decodeAvrLine "PWSTANDBY") `shouldBe` Just Off

    it "takes the echoed status as the answer to a power change" $
      answerOf (SetPower On) (decodeAvrLine "PWON") `shouldBe` Just ()

    it "does not take an event for an answer" $ do
      answerOf GetPower (decodeAvrLine "MV45") `shouldBe` Nothing
      answerOf (SetPower Off) (decodeAvrLine "SITUNER") `shouldBe` Nothing

    it "matches the whole line, not a prefix" $
      answerOf GetPower (decodeAvrLine "PWONX") `shouldBe` Nothing

  describe "sendAvrCommand" $ do
    it "asks PW? and reads the power status" $ do
      (status, sent) <- withAmp (replying (poweredAmp On)) $ \conn -> sendAvrCommand conn GetPower
      status `shouldBe` On
      sent `shouldBe` ["PW?"]

    it "reads standby as Off" $ do
      (status, _) <- withAmp (replying (poweredAmp Off)) $ \conn -> sendAvrCommand conn GetPower
      status `shouldBe` Off

    it "sends the new state and takes the echo as the answer" $ do
      ((), sent) <- withAmp (replying (poweredAmp On)) $ \conn -> sendAvrCommand conn (SetPower Off)
      sent `shouldBe` ["PWSTANDBY"]

    -- The device streams events on the same port; a volume change arriving
    -- between the question and the answer must not be taken for the answer.
    it "skips event lines until the answer arrives" $ do
      (status, _) <- withAmp (replying noisy) $ \conn -> sendAvrCommand conn GetPower
      status `shouldBe` On

    it "times out when the device stays silent" $ do
      (result, _) <- withAmp (replying (const "")) $ \conn -> try (sendAvrCommand conn GetPower)
      result `shouldBe` Left AvrConnectionTimeout

    -- A closed socket surfaces as an IO error, the kind withReconnect
    -- restarts both connections on.
    it "reports a hung-up device as an IO error" $ do
      (result, _) <- withAmp hangUp $ \conn -> try @IOException (sendAvrCommand conn GetPower)
      result `shouldSatisfy` isLeft

    -- One telnet client, one command at a time: the lock keeps a second
    -- command from reading the first one's answer.
    it "serialises concurrent commands and gives each its own answer" $ do
      answers <- newTVarIO ["PWON\r", "PWSTANDBY\r"]
      let inTurn _ = atomically $ stateTVar answers $ \case
            [] -> ("", [])
            (x : xs) -> (x, xs)
      ((a, b), sent) <- withAmp (replyingWith inTurn) $ \conn ->
        sendAvrCommand conn GetPower `concurrently` sendAvrCommand conn GetPower
      [a, b] `shouldMatchList` [On, Off]
      sent `shouldBe` ["PW?", "PW?"]

  describe "runAvrLive" $
    it "runs getPower and setPower over the connection" $ do
      amp <- statefulAmp On
      (power, sent) <- withAmp (replyingWith amp) $ \conn ->
        runEff . runAvrLive conn $ setPower Off >> getPower
      power `shouldBe` Off
      sent `shouldBe` ["PWSTANDBY", "PW?"]
 where
  noisy = \case
    "PW?" -> "MV45\rSITUNER\rPWON\r"
    _ -> ""
  hangUp _ _ = pure ()
