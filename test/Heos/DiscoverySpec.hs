module Heos.DiscoverySpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Exception (bracket)
import Control.Monad (forever)
import Data.ByteString.Char8 qualified as BS
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Word (Word8)
import GHC.Clock (getMonotonicTime)
import Heos.Discovery
import Network.Socket (
  Family (AF_INET),
  HostAddress,
  PortNumber,
  SockAddr (SockAddrInet),
  Socket,
  SocketType (Datagram),
  bind,
  close,
  defaultPort,
  defaultProtocol,
  getSocketName,
  socket,
  tupleToHostAddress,
 )
import Network.Socket.ByteString (sendTo)
import System.Timeout (timeout)
import Test.Hspec

-- IPv4 address with a port, the way recvFrom reports a sender.
addr :: PortNumber -> (Word8, Word8, Word8, Word8) -> SockAddr
addr port ip = SockAddrInet port (tupleToHostAddress ip)

-- The address alone, the way discovery reports a device: the HEOS port is
-- the connection's business.
host :: (Word8, Word8, Word8, Word8) -> HostAddress
host = tupleToHostAddress

receiver, receiver2, router, tv :: (Word8, Word8, Word8, Word8)
receiver = (192, 168, 1, 241)
receiver2 = (192, 168, 1, 242)
router = (192, 168, 1, 1)
tv = (192, 168, 1, 50)

-- What a Denon/Marantz receiver answers to our M-SEARCH.
heosReply :: (Word8, Word8, Word8, Word8) -> BS.ByteString
heosReply (a, b, c, d) =
  BS.intercalate
    "\r\n"
    [ "HTTP/1.1 200 OK"
    , "CACHE-CONTROL: max-age=180"
    , "EXT:"
    , "LOCATION: http://" <> ip <> ":60006/upnp/desc/aios_device/aios_device.xml"
    , "ST: urn:schemas-denon-com:device:ACT-Denon:1"
    , "USN: uuid:a1b2c3d4-0000-1000-8000-000000000001::urn:schemas-denon-com:device:ACT-Denon:1"
    , ""
    , ""
    ]
 where
  ip = BS.intercalate "." (map (BS.pack . show) [a, b, c, d])

-- A sloppy SSDP device that answers any M-SEARCH with its own type.
foreignReply :: BS.ByteString
foreignReply =
  BS.intercalate
    "\r\n"
    [ "HTTP/1.1 200 OK"
    , "CACHE-CONTROL: max-age=1800"
    , "LOCATION: http://192.168.1.1:5000/rootDesc.xml"
    , "ST: urn:schemas-upnp-org:device:InternetGatewayDevice:1"
    , "USN: uuid:ffffffff-0000-0000-0000-000000000000::upnp:rootdevice"
    , ""
    , ""
    ]

spec :: Spec
spec = do
  filterSpec
  collectSpec

filterSpec :: Spec
filterSpec = describe "filterDatagrams" $ do
  -- Silence: nothing arrived before the timeout.
  it "reports NoResponse when nothing arrived" $
    filterDatagrams [] `shouldBe` Left NoResponse

  -- The network works, someone answered, but it isn't HEOS. A different
  -- diagnosis from silence, and the senders are kept for the log.
  it "reports NoHeosDevice with the senders when only foreign devices answered" $
    filterDatagrams
      [ (foreignReply, addr 1900 router)
      , ("garbage", addr 49152 tv)
      ]
      `shouldBe` Left (NoHeosDevice (addr 1900 router :| [addr 49152 tv]))

  it "keeps only HEOS devices when foreign ones answered too" $
    filterDatagrams
      [ (foreignReply, addr 1900 router)
      , (heosReply receiver, addr 1900 receiver)
      , (foreignReply, addr 1900 tv)
      ]
      `shouldBe` Right (host receiver :| [])

  -- The device answers SSDP from 1900 or an ephemeral port. That port is
  -- noise: the result is the address alone.
  it "keeps the sender's address and drops its port" $ do
    filterDatagrams [(heosReply receiver, addr 1900 receiver)]
      `shouldBe` Right (host receiver :| [])
    filterDatagrams [(heosReply receiver, addr 51234 receiver)]
      `shouldBe` Right (host receiver :| [])

  -- Receivers often answer an M-SEARCH two or three times.
  it "collapses repeated replies from one receiver into one address" $
    filterDatagrams (replicate 3 (heosReply receiver, addr 1900 receiver))
      `shouldBe` Right (host receiver :| [])

  -- Duplicates may come from different source ports; they are the same
  -- device once the port is dropped.
  it "collapses replies from one receiver sent from different ports" $
    filterDatagrams
      [ (heosReply receiver, addr 1900 receiver)
      , (heosReply receiver, addr 51234 receiver)
      ]
      `shouldBe` Right (host receiver :| [])

  -- Order follows arrival and isn't part of the contract, hence shouldMatchList.
  it "returns every receiver when there are several" $
    case filterDatagrams
      [ (heosReply receiver, addr 1900 receiver)
      , (heosReply receiver2, addr 1900 receiver2)
      , (heosReply receiver, addr 1900 receiver)
      ] of
      Right found -> toList found `shouldMatchList` [host receiver, host receiver2]
      Left err -> expectationFailure ("expected two receivers, got " <> show err)

  -- The check looks at the payload, not at who sent it: a foreign reply from
  -- the receiver's own IP doesn't count as HEOS.
  it "judges by the payload, not by the sender's address" $
    filterDatagrams [(foreignReply, addr 1900 receiver)]
      `shouldBe` Left (NoHeosDevice (addr 1900 receiver :| []))

-- Review item 4. The search window is MX plus a margin, whoever is
-- talking. With a timeout per datagram alone, a chatty SSDP neighbour (a
-- TV announcing itself every 100 ms) keeps the search open for ever, and
-- the daemon never gets past discovery.
collectSpec :: Spec
collectSpec = describe "collect" $
  it "returns within the search window while datagrams keep coming" $
    withUdp $ \sock port -> withUdp $ \chatter _ ->
      withAsync (forever (sendTo chatter notify (SockAddrInet port loopback) >> threadDelay 100000)) $ \_ -> do
        startedAt <- getMonotonicTime
        got <- timeout 4000000 (collect sock)
        endedAt <- getMonotonicTime
        got `shouldSatisfy` maybe False (not . null)
        (endedAt - startedAt) `shouldSatisfy` (< 3)
 where
  notify = "NOTIFY * HTTP/1.1\r\nNT: upnp:rootdevice\r\n\r\n"

-- A UDP socket on a loopback port picked by the OS.
withUdp :: (Socket -> PortNumber -> IO a) -> IO a
withUdp k = bracket (socket AF_INET Datagram defaultProtocol) close $ \sock -> do
  bind sock (SockAddrInet defaultPort loopback)
  address <- getSocketName sock
  case address of
    SockAddrInet port _ -> k sock port
    other -> fail ("not an IPv4 address: " <> show other)

loopback :: HostAddress
loopback = tupleToHostAddress (127, 0, 0, 1)
