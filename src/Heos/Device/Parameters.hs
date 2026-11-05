module Heos.Device.Parameters where

import Control.Retry (capDelay, exponentialBackoff)
import Heos.Connection.Avr (AvrConnectionParams (..))
import Heos.Connection.Types (HeosConnectionParams (..))
import Heos.Time (Seconds (..), toMicros)

data DeviceParams = DeviceParams
  { heos :: HeosConnectionParams
  , avr :: AvrConnectionParams
  }

defaultParams :: DeviceParams
defaultParams =
  DeviceParams
    { heos =
        HeosConnectionParams
          { commandTimeout = Seconds 5
          , heartbeatDelay = Seconds 30
          , maxLineLength = 1024 * 1024
          , port = 1255
          , retryPolicy = (capDelay . toMicros) (Seconds 60) $ (exponentialBackoff . toMicros) (Seconds 1)
          }
    , avr =
        AvrConnectionParams
          { port = 23
          , requestTimeout = Seconds 1
          }
    }