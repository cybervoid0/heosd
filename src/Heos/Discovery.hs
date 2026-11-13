module Heos.Discovery (discover, DiscoveryError (..), filterDatagrams, collect) where

import Control.Exception (bracket, catch, throwIO)
import Control.Monad (forever)
import Data.ByteString.Char8 qualified as BS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty, nub)
import Data.Maybe (mapMaybe)
import Heos.Error (DiscoveryError (..))
import Network.Socket (
  Family (AF_INET),
  HostAddress,
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

type DiscoveryResult = Either DiscoveryError (NonEmpty HostAddress)
type Datagram = (BS.ByteString, SockAddr)

-- | SSDP multicast group and port
multicastAddr :: SockAddr
multicastAddr = SockAddrInet 1900 (tupleToHostAddress (239, 255, 255, 250))

-- | MX: how long devices may wait before replying, in seconds
searchDelay :: Int
searchDelay = 1

-- | HEOS URN (Uniform Resource Name)
heosDeviceType :: BS.ByteString
heosDeviceType = "urn:schemas-denon-com:device:ACT-Denon:1"

mSearchData :: BS.ByteString
mSearchData =
  BS.intercalate
    "\r\n"
    [ "M-SEARCH * HTTP/1.1"
    , "HOST: " <> bsShow multicastAddr
    , "MAN: \"ssdp:discover\""
    , "MX: " <> bsShow searchDelay
    , "ST: " <> bsShow heosDeviceType
    , ""
    , ""
    ]
 where
  bsShow val = BS.pack (show val)

-- | Finds HEOS devices in the local network
discover :: IO (NonEmpty HostAddress)
discover =
  bracket open close handleSocket `catch` (throwIO . SocketFailure) >>= either throwIO pure . filterDatagrams
 where
  open = socket AF_INET Datagram defaultProtocol
  handleSocket sock = sendAllTo sock mSearchData multicastAddr >> collect sock

filterDatagrams :: [Datagram] -> DiscoveryResult
filterDatagrams datagrams = case (mbResults, nonEmpty others) of
  (Just res, _) -> Right . nub $ res
  (Nothing, Just nonHeos) -> Left $ NoHeosDevice (snd <$> nonHeos)
  (Nothing, Nothing) -> Left NoResponse
 where
  (heos, others) = partition isHeos datagrams
  isHeos (msg, _) = heosDeviceType `BS.isInfixOf` msg
  mbResults = nonEmpty . mapMaybe go $ heos
  go (_, SockAddrInet _ ipV4Addr) = Just ipV4Addr
  go _ = Nothing

collect :: Socket -> IO [Datagram]
collect sock = do
  acc <- newIORef []
  _ <- timeout delay . forever . go $ acc
  readIORef acc
 where
  delay = searchDelay * 1000000 + 100000
  bufferSize = 4096 -- bytes
  go acc = recvFrom sock bufferSize >>= modifyIORef' acc . (:)
