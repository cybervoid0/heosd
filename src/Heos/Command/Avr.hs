module Heos.Command.Avr where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Heos.Types (OnOff (..))

data AvrCommand a where
  GetPower :: AvrCommand OnOff
  SetPower :: OnOff -> AvrCommand ()

renderAvrCommand :: AvrCommand a -> ByteString
renderAvrCommand = encodeUtf8 . (<> "\r") . go
 where
  go :: AvrCommand a -> Text
  go = \case
    GetPower -> "PW?"
    SetPower pw -> case pw of
      On -> "PWON"
      Off -> "PWSTANDBY"