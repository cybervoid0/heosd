module Heos.DiscoverySpec (spec) where

import Data.ByteString.Char8 qualified as BS
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Word (Word8)
import Heos.Discovery.Internal
import Network.Socket (PortNumber, SockAddr (SockAddrInet), tupleToHostAddress)
import Test.Hspec

-- IPv4 address with a port, the way recvFrom reports a sender.
addr :: PortNumber -> (Word8, Word8, Word8, Word8) -> SockAddr
addr port ip = SockAddrInet port (tupleToHostAddress ip)

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
spec = describe "filterDatagrams" $ do
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
      `shouldBe` Right (addr 1255 receiver :| [])

  -- The device answers SSDP from 1900 or an ephemeral port, but the HEOS CLI
  -- always listens on 1255.
  it "replaces the sender's port with the HEOS CLI port 1255" $ do
    filterDatagrams [(heosReply receiver, addr 1900 receiver)]
      `shouldBe` Right (addr 1255 receiver :| [])
    filterDatagrams [(heosReply receiver, addr 51234 receiver)]
      `shouldBe` Right (addr 1255 receiver :| [])

  -- Receivers often answer an M-SEARCH two or three times.
  it "collapses repeated replies from one receiver into one address" $
    filterDatagrams (replicate 3 (heosReply receiver, addr 1900 receiver))
      `shouldBe` Right (addr 1255 receiver :| [])

  -- Duplicates may come from different source ports; they are the same
  -- device once the port is normalised.
  it "collapses replies from one receiver sent from different ports" $
    filterDatagrams
      [ (heosReply receiver, addr 1900 receiver)
      , (heosReply receiver, addr 51234 receiver)
      ]
      `shouldBe` Right (addr 1255 receiver :| [])

  -- Order follows arrival and isn't part of the contract, hence shouldMatchList.
  it "returns every receiver when there are several" $
    case filterDatagrams
      [ (heosReply receiver, addr 1900 receiver)
      , (heosReply receiver2, addr 1900 receiver2)
      , (heosReply receiver, addr 1900 receiver)
      ] of
      Right found -> toList found `shouldMatchList` [addr 1255 receiver, addr 1255 receiver2]
      Left err -> expectationFailure ("expected two receivers, got " <> show err)

  -- The check looks at the payload, not at who sent it: a foreign reply from
  -- the receiver's own IP doesn't count as HEOS.
  it "judges by the payload, not by the sender's address" $
    filterDatagrams [(foreignReply, addr 1900 receiver)]
      `shouldBe` Left (NoHeosDevice (addr 1900 receiver :| []))
