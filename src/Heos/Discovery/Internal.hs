module Heos.Discovery.Internal where

import Control.Exception (IOException, bracket, try)
import Control.Retry (RetryStatus, constantDelay, limitRetries, retrying)
import Data.ByteString.Char8 qualified as BS
import Data.Either (isLeft)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty, nub)
import Data.Maybe (mapMaybe)
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

data DiscoveryError
  = -- socket/sendAllTo failed: no network, no route to multicast, EPERM
    SocketFailure IOException
  | -- not a single datagram arrived before collect timed out
    NoResponse
  | -- replies arrived, but none is from ACT-Denon (foreign SSDP devices:
    -- router, TV, Chromecast); the addresses are useful in the log
    NoHeosDevice (NonEmpty SockAddr)
  deriving (Show, Eq)

type DiscoveryResult = Either DiscoveryError (NonEmpty SockAddr)
type Datagram = (BS.ByteString, SockAddr)

-- | Finds HEOS devices in the local network
discover :: IO (Either DiscoveryError (NonEmpty SockAddr))
discover = retrying policy shouldRetry discoverOnce
 where
  policy = constantDelay 500000 <> limitRetries 2 -- 0.5 s pause, up to 2 retries (3 attempts)
  -- retry only on Left, i.e. when there is an error
  shouldRetry _ = pure . isLeft

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

discoverOnce :: RetryStatus -> IO DiscoveryResult
discoverOnce _ =
  either (Left . SocketFailure) filterDatagrams
    <$> try @IOException (bracket open close handleSocket)
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
  heosPort = 1255
  go (_, SockAddrInet _ ipV4Addr) = Just $ SockAddrInet heosPort ipV4Addr
  go _ = Nothing

collect :: Socket -> IO [Datagram]
collect sock = go []
 where
  delay = searchDelay * 1000000 + 100000
  bufferSize = 4096 -- bytes
  go acc = do
    -- timeout stops waiting if nothing arrived within 1.5 s
    mbDatagram <- timeout delay $ recvFrom sock bufferSize
    case mbDatagram of
      Nothing -> pure acc
      Just pair -> go (pair : acc)
