module Heos.Connection.Avr where

import Control.Concurrent (MVar, newMVar, withMVar)
import Control.Exception (Exception (displayException), IOException, handle, throwIO, try)
import Control.Monad ((>=>))
import Control.Monad.Catch (bracket, bracketOnError)
import Data.ByteString.Char8 qualified as B
import Heos.Command.Avr (AvrCommand (..), renderAvrCommand)
import Heos.Envelope.Avr (AvrResponse (..), decodeAvrLine)
import Heos.Error (AvrError (..), ConnectionError (ConnectionLost))
import Heos.Logger (Logger (..), Severity (Warn), logged)
import Heos.Time (Seconds, toMicros)
import Network.Socket (Family (AF_INET), HostAddress, PortNumber, SockAddr (..), SocketType (Stream), socketToHandle)
import Network.Socket qualified as S
import System.IO (Handle, IOMode (ReadWriteMode), hClose, hFlush, hGetChar, hSetBinaryMode)
import System.Timeout (timeout)

-- | What the telnet connection is opened with.
data AvrConnectionParams = AvrConnectionParams
  { port :: PortNumber
  , requestTimeout :: Seconds
  , logger :: Logger
  }

data AvrConnection = AvrConnection
  { lock :: MVar Handle
  , params :: AvrConnectionParams
  }

withAvr :: AvrConnectionParams -> HostAddress -> (Maybe AvrConnection -> IO a) -> IO a
withAvr params addr act = bracket open (mapM_ hClose) (traverse toConnection >=> act)
 where
  toConnection h = do
    lock <- newMVar h
    pure $ AvrConnection{lock, params}

  open = bracketOnError (S.socket AF_INET Stream S.defaultProtocol) S.close $ \s ->
    (try @IOException $ S.connect s sockAddr) >>= \case
      Left e -> do
        logAvrError e
        S.close s
        pure Nothing
      Right () -> do
        h <- socketToHandle s ReadWriteMode
        hSetBinaryMode h True
        pure $ Just h

  sockAddr = SockAddrInet params.port addr
  logAvrError e = params.logger.logAt Warn $ "AVR: cannot connect to " <> show sockAddr <> ": " <> displayException e

sendAvrCommand :: AvrConnection -> AvrCommand a -> IO a
sendAvrCommand conn cmd = handle (\(_ :: IOException) -> throwIO ConnectionLost) $ withMVar conn.lock $ \h ->
  logged conn.params.logger ("AVR " <> show cmd) $ do
    B.hPut h (renderAvrCommand cmd)
    hFlush h
    timeout (toMicros conn.params.requestTimeout) (awaitAnswer h) >>= maybe (throwIO AvrNoReply) pure
 where
  -- события вроде MV45 пропускаем, ждём строку, которая отвечает на нашу команду
  awaitAnswer h = do
    res <- decodeAvrLine <$> readCR h
    maybe (awaitAnswer h) pure (answerOf cmd res)

answerOf :: AvrCommand a -> AvrResponse -> Maybe a
answerOf GetPower (PowerStatus pw) = Just pw
answerOf (SetPower _) (PowerStatus _) = Just () -- устройство эхом шлёт PWON / PWSTANDBY
answerOf _ _ = Nothing

readCR :: Handle -> IO B.ByteString
readCR h = go []
 where
  go acc = do
    c <- hGetChar h
    if c == '\r' then pure (B.pack (reverse acc)) else go (c : acc)
