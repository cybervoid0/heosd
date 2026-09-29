module Heos.Discovery (discover) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.ByteString.Char8 qualified as BS
import Network.Socket (
  Family (AF_INET),
  SockAddr (SockAddrInet),
  Socket,
  SocketType (Datagram),
  close,
  defaultProtocol,
  socket,
  tupleToHostAddress,
 )
import Network.Socket.ByteString (recvFrom, sendAllTo)
import System.Timeout (timeout)

discover :: IO (Maybe SockAddr)
discover = go maxRetries
 where
  maxRetries = 3
  delay = 500000
  go :: Int -> IO (Maybe SockAddr)
  go 0 = pure Nothing
  go n = do
    found <- discoverOnce
    case found of
      justAddr@(Just _) -> pure justAddr
      Nothing -> do
        threadDelay delay
        go (n - 1)

mSearchData :: BS.ByteString
mSearchData =
  BS.intercalate
    "\r\n"
    [ "M-SEARCH * HTTP/1.1"
    , "HOST: 239.255.255.250:1900"
    , "MAN: \"ssdp:discover\""
    , "MX: 1"
    , "ST: urn:schemas-denon-com:device:ACT-Denon:1"
    , ""
    , ""
    ]

discoverOnce :: IO (Maybe SockAddr)
discoverOnce = do
  content <-
    bracket
      (socket AF_INET Datagram defaultProtocol)
      close
      handleSocket
  case content of
    ((_, SockAddrInet _ hostAddr) : _) -> pure . Just $ SockAddrInet 1255 hostAddr
    _ -> pure Nothing

handleSocket :: Socket -> IO [(BS.ByteString, SockAddr)]
handleSocket sock = do
  let multicastAddr = SockAddrInet 1900 (tupleToHostAddress (239, 255, 255, 250))
  sendAllTo sock mSearchData multicastAddr
  collect sock

collect :: Socket -> IO [(BS.ByteString, SockAddr)]
collect sock = go []
 where
  delay = 1500000 -- 1.5 sec
  bufferSize = 4096 -- bytes
  go acc = do
    r <- timeout delay $ recvFrom sock bufferSize
    case r of
      Nothing -> pure acc
      Just pair -> go (pair : acc)
