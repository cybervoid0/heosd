-- | The system side of the protocol (spec 4.1): the account, and the on/off
--   switch that events, mute and the amplifier's power all share.
module Heos.Types.System where

import Data.Text (Text)
import Data.Text qualified as T
import Heos.ToParam (ToParam (..))

data Credentials = Credentials {login :: Text, password :: Text} deriving (Eq)
instance Show Credentials where
  show (Credentials login _) = "Login=" <> T.unpack login <> " Password=<hidden>"

data OnOff = On | Off deriving (Show, Eq, Ord)
instance ToParam OnOff where
  toParam status = case status of
    On -> "on"
    Off -> "off"
