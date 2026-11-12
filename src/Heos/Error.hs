module Heos.Error where

import Control.Exception (Exception (..), IOException, SomeException)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as T
import Network.Socket (SockAddr)

data DiscoveryError
  = -- socket/sendAllTo failed: no network, no route to multicast, EPERM
    SocketFailure IOException
  | -- not a single datagram arrived before collect timed out
    NoResponse
  | -- replies arrived, but none is from ACT-Denon (foreign SSDP devices:
    -- router, TV, Chromecast); the addresses are useful in the log
    NoHeosDevice (NonEmpty SockAddr)
  deriving (Show, Eq)
instance Exception DiscoveryError where
  displayException (SocketFailure e) = "Socket failure: " <> show e
  displayException NoResponse = "No response"
  displayException (NoHeosDevice addr) = "Found non-Heos device: " <> show addr

data ConnectionError
  = ConnectionLost
  | LineTooLong
  | NoPlayers
  | BackgroundFailure SomeException
  deriving (Show)
instance Exception ConnectionError where
  displayException ConnectionLost = "Heos device closed the connection"
  displayException LineTooLong = "Heos device sent a line longer than allowed"
  displayException NoPlayers = "No Heos players in the local network"
  displayException (BackgroundFailure e) = "A background thread died: " <> displayException e

data CommandError
  = -- | eid / text
    Refused (Maybe Int) (Maybe Text) -- было HeosError: result=fail
  | Unreadable Text -- было ProtocolError: ответ не разобрался
  | NoReply -- было CommandTimeout
  deriving (Show, Eq)
instance Exception CommandError where
  displayException (Refused eid text) = "Heos device refused: " <> maybe "" T.unpack text <> maybe "" ((" eid=" <>) . show) eid
  displayException (Unreadable msg) = "Heos device sent an unreadable reply: " <> T.unpack msg
  displayException NoReply = "Heos device did not answer in time"

data AvrError = AvrNoReply deriving (Show, Eq)
instance Exception AvrError where
  displayException AvrNoReply = "Heos device did not answer via AVR in time"