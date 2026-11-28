module Heos.Connection.Avr (sendAvrCommand, withAvr, AvrConnection (..), AvrConnectionParams (..), answerOf) where

import Control.Concurrent (MVar, newMVar, withMVar)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (TChan, TVar, atomically, check, dupTChan, newBroadcastTChanIO, newTVarIO, orElse, readTChan, readTVar, writeTChan, writeTVar)
import Control.Exception (Exception (displayException), IOException, bracket, finally, handle, throwIO, try)
import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Void (Void, absurd)
import Heos.Command.Avr (AvrCommand (..), renderAvrCommand)
import Heos.Connection.Framing (Pending (..), emptyPending, feedChunk)
import Heos.Envelope.Avr (AvrResponse (..), decodeAvrLine)
import Heos.Error (AvrError (..), ConnectionError (ConnectionLost, LineTooLong))
import Heos.Logger (Logger (..), Severity (..), logged)
import Heos.Time (Seconds, toMicros)
import Network.Socket (Family (AF_INET), HostAddress, PortNumber, SockAddr (SockAddrInet), Socket, SocketType (Stream))
import Network.Socket qualified as S
import Network.Socket.ByteString (recv, sendAll)
import System.Timeout (timeout)

-- | What ends a line on the telnet wire.
lineEnd :: ByteString
lineEnd = "\r"

-- | What the telnet connection is opened with.
data AvrConnectionParams = AvrConnectionParams
  { port :: PortNumber
  , requestTimeout :: Seconds
  , maxLineLength :: Int
  , chunkLength :: Int
  , logger :: Logger
  }

-- | An open telnet line: the socket, the lines the reader hands out,
--   whether the reader is still running, and a lock so that only one
--   command is in flight at a time.
data AvrConnection = AvrConnection
  { socket :: Socket
  , linesChannel :: TChan AvrResponse
  , alive :: TVar Bool
  , lock :: MVar ()
  , params :: AvrConnectionParams
  }

-- | Connect a socket to the amplifier's telnet port and close it when
--   the action ends. A refused connect is logged and gives 'Nothing'.
withAvrSocket :: AvrConnectionParams -> HostAddress -> (Maybe Socket -> IO a) -> IO a
withAvrSocket params hostAddr act = bracket (S.socket AF_INET Stream S.defaultProtocol) S.close $ \sock ->
  try @IOException (S.connect sock sockAddr) >>= \case
    Left e -> logAvrError e >> act Nothing
    Right () -> act (Just sock)
 where
  sockAddr = SockAddrInet params.port hostAddr
  logAvrError e = params.logger.logAt Warn $ "AVR: cannot connect to " <> show sockAddr <> ": " <> displayException e

-- | Open the telnet line and run the action over it, with the reader
--   beside it. 'Nothing' when the port refuses. If the reader dies, the
--   action is cancelled and its error comes out of here.
withAvr :: AvrConnectionParams -> HostAddress -> (Maybe AvrConnection -> IO a) -> IO a
withAvr params hostAddr act = withAvrSocket params hostAddr $ \case
  Nothing -> act Nothing
  Just socket -> do
    linesChannel <- newBroadcastTChanIO
    alive <- newTVarIO True
    lock <- newMVar ()
    let avrConn = AvrConnection{socket, linesChannel, alive, lock, params}
    res <- race (avrReader avrConn) (act $ Just avrConn)
    either absurd pure res

-- | The only thread that reads the socket. Cuts the stream into lines
--   and broadcasts each one. On hang-up it marks the line dead and
--   throws 'ConnectionLost'.
avrReader :: AvrConnection -> IO Void
avrReader conn = withErrHandle `finally` failAll
 where
  withErrHandle = handle @IOException (const $ throwIO ConnectionLost) $ go emptyPending
  go pending = do
    chunk <- recv conn.socket conn.params.chunkLength
    when (BS.null chunk) $ throwIO ConnectionLost
    let (complete, pending') = feedChunk lineEnd pending chunk
    when (pending'.size > conn.params.maxLineLength) $ throwIO LineTooLong
    forM_ complete route
    go pending'
  route bs = do
    let avrResp = decodeAvrLine bs
    atomically $ writeTChan conn.linesChannel avrResp
  failAll = atomically $ writeTVar conn.alive False

-- | Send a command and wait for its answer, skipping events. Throws
--   'AvrNoReply' on timeout and 'ConnectionLost' if the line drops.
sendAvrCommand :: AvrConnection -> AvrCommand a -> IO a
sendAvrCommand conn cmd =
  handle @IOException (const $ throwIO ConnectionLost) $ withMVar conn.lock $ \_ ->
    logged conn.params.logger ("AVR " <> show cmd) $ do
      linesChan <- atomically $ dupTChan conn.linesChannel
      sendAll conn.socket (renderAvrCommand cmd)
      timeout (toMicros conn.params.requestTimeout) (awaitAnswer linesChan) >>= maybe (throwIO AvrNoReply) pure
 where
  awaitAnswer linesChan = do
    next <-
      atomically $
        (Just <$> readTChan linesChan) `orElse` (readTVar conn.alive >>= check . not >> pure Nothing)
    case next of
      Nothing -> throwIO ConnectionLost
      Just res -> maybe (awaitAnswer linesChan) pure (answerOf cmd res)

-- | Whether this line answers this command, and with what.
answerOf :: AvrCommand a -> AvrResponse -> Maybe a
answerOf GetPower (PowerStatus pw) = Just pw
answerOf (SetPower _) (PowerStatus _) = Just () -- устройство эхом шлёт PWON / PWSTANDBY
answerOf _ _ = Nothing
