module Heos.Connection.Avr where

import Control.Concurrent (MVar, newMVar, withMVar)
import Control.Exception (Exception (displayException), throwIO)
import Control.Monad.Catch (bracket, bracketOnError)
import Data.ByteString.Char8 qualified as B
import Heos.Command.Avr (AvrCommand (..), renderAvrCommand)
import Heos.Envelope.Avr (AvrResponse (..), decodeAvrLine)
import Heos.Time (toMicros)
import Heos.Types
import Network.Socket (Family (AF_INET), HostAddress, SockAddr (..), SocketType (Stream), socketToHandle)
import Network.Socket qualified as S
import System.IO (Handle, IOMode (ReadWriteMode), hClose, hFlush, hGetChar, hSetBinaryMode)
import System.Timeout (timeout)

data AvrConnectionError = AvrConnectionTimeout deriving (Show, Eq)
instance Exception AvrConnectionError where
  displayException AvrConnectionTimeout = "the device did not answer via AVR in time"

data AvrConnection = AvrConnection
  { lock :: MVar Handle
  , params :: AvrConnectionParams
  }

withAvr :: AvrConnectionParams -> HostAddress -> (AvrConnection -> IO a) -> IO a
withAvr params addr act = bracket open hClose $ \handle -> do
  hBox <- newMVar handle
  act (AvrConnection hBox params)
 where
  open = bracketOnError (S.socket AF_INET Stream S.defaultProtocol) S.close $ \s -> do
    S.connect s (SockAddrInet params.port addr)
    handle <- socketToHandle s ReadWriteMode
    hSetBinaryMode handle True
    pure handle

sendAvrCommand :: AvrConnection -> AvrCommand a -> IO a
sendAvrCommand conn cmd = withMVar conn.lock $ \h -> do
  B.hPut h (renderAvrCommand cmd)
  hFlush h
  timeout (toMicros conn.params.requestTimeout) (awaitAnswer h) >>= maybe (throwIO AvrConnectionTimeout) pure
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
