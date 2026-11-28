module Heos.AvrSpec (spec) where

import Control.Concurrent (newEmptyMVar, putMVar, readMVar, threadDelay)
import Control.Concurrent.Async (concurrently, wait, withAsync)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO, stateTVar, writeTVar)
import Control.Exception (bracket, try)
import Control.Monad (forM_, forever, unless, void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Effectful (runEff)
import Heos.Command.Avr (AvrCommand (..), renderAvrCommand)
import Heos.Connection.Avr (AvrConnection, AvrConnectionParams (..), sendAvrCommand, withAvr)
import Heos.Error (AvrError (..), ConnectionError (..))
import Heos.Effect.Avr (getPower, turnOff)
import Heos.Envelope.Avr (AvrResponse (..), decodeAvrLine)
import Heos.Handler.AvrLive (runAvrLive)
import Heos.Logger (Logger, Severity (..), capturing)
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
import System.Timeout (timeout)
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
paramsFor port = AvrConnectionParams{port, requestTimeout = Seconds 0.2, maxLineLength = 1024, chunkLength = 256, logger = mempty}

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
withAmp = withAmpLoggingTo mempty

-- 'withAmp' with the connection writing its log to this logger.
withAmpLoggingTo :: Logger -> Script -> (AvrConnection -> IO a) -> IO (a, [ByteString])
withAmpLoggingTo spy script act = bracket (socket AF_INET Stream defaultProtocol) close $ \srv -> do
  setSocketOption srv ReuseAddr 1
  bind srv (SockAddrInet defaultPort loopback)
  listen srv 1
  port <- portOf <$> getSocketName srv
  seen <- newTVarIO []
  withAsync (bracket (fst <$> accept srv) close (script seen)) $ \amp -> do
    result <- withAvr (paramsFor port){logger = spy} loopback (maybe (fail "the telnet line did not open") act)
    wait amp
    sent <- reverse <$> readTVarIO seen
    pure (result, sent)
 where
  portOf (SockAddrInet port _) = port
  portOf other = error ("not an IPv4 address: " <> show other)

-- A port nobody listens on: picked by the OS and released again, so a
-- connect to it is refused at once.
freePort :: IO PortNumber
freePort = bracket (socket AF_INET Stream defaultProtocol) close $ \srv -> do
  bind srv (SockAddrInet defaultPort loopback)
  getSocketName srv >>= \case
    SockAddrInet port _ -> pure port
    other -> fail ("not an IPv4 address: " <> show other)

-- The daemon's shape: one thread holds the line open (the supervisor's),
-- commands come from another (the server's). Opens the line to the
-- amplifier and holds it until it drops; meanwhile sends the command from
-- a thread of its own. Gives back how the line ended and how the command
-- did, each on its own: inside the action the command's error and the
-- reader's would race out of 'withAvr', and only one would be seen.
commandFromOutside :: Logger -> Script -> AvrCommand a -> IO (Either ConnectionError (), Either ConnectionError a)
commandFromOutside spy script cmd = do
  handed <- newEmptyMVar
  withAsync (readMVar handed >>= \conn -> try @ConnectionError (sendAvrCommand conn cmd)) $ \command -> do
    line <- try @ConnectionError . bounded $ withAmpLoggingTo spy script $ \conn ->
      putMVar handed conn >> holdForever
    answer <- wait command
    pure (void line, answer)

-- Keep the line until something from outside ends it.
holdForever :: IO ()
holdForever = forever (threadDelay maxBound)

-- A reader that misses the hang-up would hold the line for good; fail the
-- test instead of hanging the suite.
bounded :: IO a -> IO a
bounded io = timeout 2000000 io >>= maybe (fail "timed out after 2 s") pure

-- Hold the socket until the daemon hangs up.
holding :: Socket -> IO ()
holding sock = void (recv sock 1024)

-- What was logged at Warn or above. The tests do not tell a warning from
-- an error; that line is the library's to draw.
complaints :: [(Severity, String)] -> [String]
complaints entries = [msg | (sev, msg) <- entries, sev >= Warn]

spec :: Spec
spec = do
  describe "renderAvrCommand" $ do
    it "ends every command with a carriage return" $ do
      renderAvrCommand GetPower `shouldBe` "PW?\r"
      renderAvrCommand (SetPower On) `shouldBe` "PWON\r"
      renderAvrCommand (SetPower Off) `shouldBe` "PWSTANDBY\r"

  describe "decodeAvrLine" $ do
    it "reads the power status" $ do
      decodeAvrLine "PWON" `shouldBe` PowerStatus On
      decodeAvrLine "PWSTANDBY" `shouldBe` PowerStatus Off

    it "keeps an event it does not know as it came" $
      decodeAvrLine "MV45" `shouldBe` AvrResponseUnknown "MV45"

    it "matches the whole line, not a prefix" $
      decodeAvrLine "PWONX" `shouldBe` AvrResponseUnknown "PWONX"

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
      result `shouldBe` Left AvrNoReply

    -- The answer may come in two pieces; the reader joins them.
    it "reads an answer that arrives cut in two" $ do
      let split _ sock = do
            void (recv sock 1024)
            sendAll sock "PWSTA" >> threadDelay 20000 >> sendAll sock "NDBY\r"
            holding sock
      (status, _) <- withAmp split $ \conn -> sendAvrCommand conn GetPower
      status `shouldBe` Off

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

  -- Stage 34. The telnet side writes to the same log as the HEOS side;
  -- its lines must say they are about telnet, or a refused telnet port
  -- reads as "the HEOS device closed the connection".
  describe "what the telnet connection reports" $ do
    it "notes a served command below Warn, by name" $ do
      (spy, seen) <- capturing
      _ <- withAmpLoggingTo spy (replying (poweredAmp On)) $ \conn -> sendAvrCommand conn GetPower
      entries <- seen
      complaints entries `shouldBe` []
      map snd entries `shouldSatisfy` any (\msg -> "AVR" `isInfixOf` msg && "GetPower" `isInfixOf` msg)

    it "complains about a command the device left unanswered" $ do
      (spy, seen) <- capturing
      _ <- withAmpLoggingTo spy (replying (const "")) $ \conn -> try @AvrError (sendAvrCommand conn (SetPower Off))
      said <- complaints <$> seen
      said `shouldSatisfy` any (\msg -> "AVR" `isInfixOf` msg && "SetPower" `isInfixOf` msg)

    it "complains about a command cut short by the device hanging up" $ do
      (spy, seen) <- capturing
      _ <- commandFromOutside spy hangUpOnCommand GetPower
      complaints <$> seen >>= (`shouldSatisfy` any ("GetPower" `isInfixOf`))

    it "does not note a failed command as served" $ do
      (spy, seen) <- capturing
      _ <- withAmpLoggingTo spy (replying (const "")) $ \conn -> try @AvrError (sendAvrCommand conn GetPower)
      entries <- seen
      [msg | (sev, msg) <- entries, sev < Warn] `shouldBe` []

    -- The telnet port can be closed while HEOS works: network control
    -- switched off in the amplifier's settings. That is no reason to fail:
    -- the action runs without a telnet line, and the complaint names the
    -- address that refused.
    it "hands over no connection, and complains with the address, when the telnet port is closed" $ do
      (spy, seen) <- capturing
      port <- freePort
      opened <- withAvr (paramsFor port){logger = spy} loopback (pure . isJust)
      opened `shouldBe` False
      said <- complaints <$> seen
      said `shouldSatisfy` any (\msg -> "127.0.0.1" `isInfixOf` msg && show port `isInfixOf` msg)

    -- The amplifier holds the line: had it hung up, the reader could see
    -- that before the action ended, and the test would hang on a race.
    it "has nothing to say about a connection that opens and closes without a command" $ do
      (spy, seen) <- capturing
      _ <- withAmpLoggingTo spy (replying (const "")) $ \_ -> pure ()
      seen `shouldReturn` []

  -- Stage 34b. One thread reads the telnet socket all the time, not only
  -- while a command waits: it cuts the stream into lines, hands them to
  -- whoever waits, and notices the amplifier hanging up at once.
  describe "the telnet reader" $ do
    -- No command in flight, and the drop is still seen: withAvr ends with
    -- the error the reconnect loop restarts on.
    it "notices the amplifier hanging up without a command" $ do
      result <- try @ConnectionError . bounded $ withAmp hangUp (const holdForever)
      result `shouldSatisfy` either lost (const False)

    -- The command waits in another thread. It learns of the drop at once,
    -- as a lost connection: not AvrNoReply, which would say the amplifier
    -- was asked and kept quiet.
    it "fails a command waiting for its answer when the line drops" $ do
      (line, answer) <- commandFromOutside mempty hangUpOnCommand GetPower
      answer `shouldSatisfy` either lost (const False)
      line `shouldSatisfy` either lost (const False)

    -- A command sent from the action itself: its error and the reader's
    -- race out of withAvr. Both say the same, so whichever wins, the
    -- reconnect loop gets the error it restarts on.
    it "ends with a lost connection whichever notices the drop first" $ do
      result <- try @ConnectionError . bounded $ withAmp hangUpOnCommand (\conn -> sendAvrCommand conn GetPower)
      result `shouldSatisfy` either lost (const False)

    -- Events come whether anyone asked or not. With no command waiting
    -- they go nowhere, and the next answer is still the next answer.
    it "drops events that come between commands" $ do
      let chatty seen sock = sendAll sock "MV45\rSITUNER\r" >> replying (poweredAmp Off) seen sock
      (status, _) <- withAmp chatty $ \conn -> threadDelay 50000 >> sendAvrCommand conn GetPower
      status `shouldBe` Off

    -- A line with no end would grow the buffer for good.
    it "gives up on a line that never ends" $ do
      let endless _ sock = sendAll sock (BC.replicate 2000 'x') >> holding sock
      result <- try @ConnectionError . bounded $ withAmp endless (const holdForever)
      result `shouldSatisfy` either tooLong (const False)

  describe "runAvrLive" $ do
    it "runs getPower and setPower over the connection" $ do
      amp <- statefulAmp On
      (power, sent) <- withAmp (replyingWith amp) $ \conn ->
        runEff . runAvrLive (Just conn) $ turnOff >> getPower
      power `shouldBe` Off
      sent `shouldBe` ["PWSTANDBY", "PW?"]

    -- No telnet line: a power command fails with its own error, not with
    -- AvrNoReply, which would say the amplifier was asked and kept quiet.
    it "refuses a power command when there is no telnet line" $
      runEff (runAvrLive Nothing getPower) `shouldThrow` (== AvrUnavailable)

    -- The refusal comes with the command, not with the handler: a program
    -- that never asks for power runs as if the line were there.
    it "runs a program that sends no power command" $
      runEff (runAvrLive Nothing (pure "done")) `shouldReturn` ("done" :: String)
 where
  noisy = \case
    "PW?" -> "MV45\rSITUNER\rPWON\r"
    _ -> ""
  hangUp _ _ = pure ()
  -- Take the command, then hang up without an answer.
  hangUpOnCommand _ sock = void (recv sock 1024)
  lost = \case
    ConnectionLost -> True
    _ -> False
  tooLong = \case
    LineTooLong -> True
    _ -> False
