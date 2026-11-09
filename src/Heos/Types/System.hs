-- | The system side of the protocol (spec 4.1): the account, and the on/off
--   switch that events, mute and the amplifier's power all share.
module Heos.Types.System where

import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Heos.ToParam (ToParam (..))

data Credentials = Credentials {login :: Text, password :: Text} deriving (Eq)
instance Show Credentials where
  show (Credentials login _) = "Login=" <> T.unpack login <> " Password=<hidden>"

data OnOff = On | Off deriving (Show, Eq, Ord, Enum, Bounded)
instance ToParam OnOff where
  toParam status = case status of
    On -> "on"
    Off -> "off"

-- | The wire spelling back: "on" / "off", as in the message of get_mute.
parseOnOff :: Text -> Maybe OnOff
parseOnOff t = find ((== t) . toParam) [minBound .. maxBound]

-- | What check_update answers, in its payload: {"update": "update_none"}.
data UpdateState = UpdateNone | UpdateExist deriving (Show, Eq)

parseUpdateState :: Text -> Maybe UpdateState
parseUpdateState = \case
  "update_none" -> Just UpdateNone
  "update_exist" -> Just UpdateExist
  _ -> Nothing
