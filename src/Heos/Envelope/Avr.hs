module Heos.Envelope.Avr where

import Data.ByteString (ByteString)
import Heos.Types (OnOff (..))

{-
Ответ который приходит по протоколу telnet и его парсинг
 -}

data AvrResponse = PowerStatus OnOff | AvrResponseUnknown ByteString deriving (Show, Eq)

decodeAvrLine :: ByteString -> AvrResponse
decodeAvrLine = \case
  "PWON" -> PowerStatus On
  "PWSTANDBY" -> PowerStatus Off
  other -> AvrResponseUnknown other
