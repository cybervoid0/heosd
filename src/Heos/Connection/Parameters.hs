-- | Knobs of a connection. Fixed values for now; the config (stage 33) will
--   fill them in. Kept as a record so tests and the config can pass their
--   own instead of the defaults.
module Heos.Connection.Parameters (ConnectionParams (..), connParams, reconnectionPolicy, AvrParams (..), avrParams) where

import Control.Retry (RetryPolicyM, capDelay, exponentialBackoff)
import Heos.Time (Seconds (..), toMicros)
import Heos.Types
import Network.Socket (PortNumber)

data ConnectionParams = ConnectionParams
  { commandTimeout :: Seconds
  -- ^ how long a command may wait for its reply
  , heartbeatDelay :: Seconds
  -- ^ pause between heartbeats on each socket
  , playerId :: PlayerId
  -- ^ the player the daemon controls
  , maxLineLength :: Int
  -- ^ how many bytes the device may send without a "\r\n" before the link
  --   counts as broken; keeps the reader's buffer bounded
  , port :: PortNumber
  , retryPolicy :: RetryPolicyM IO
  }

connParams :: ConnectionParams
connParams =
  ConnectionParams
    { commandTimeout = Seconds 5
    , heartbeatDelay = Seconds 30
    , playerId = PlayerId (-1245882071)
    , maxLineLength = 1024 * 1024
    , port = 1255
    , retryPolicy = reconnectionPolicy
    }

reconnectionPolicy :: RetryPolicyM IO
reconnectionPolicy = (capDelay . toMicros) (Seconds 60) $ (exponentialBackoff . toMicros) (Seconds 1)

data AvrParams = AvrParams
  { port :: PortNumber
  , requestTimeout :: Seconds
  }

avrParams :: AvrParams
avrParams =
  AvrParams
    { port = 23
    , requestTimeout = Seconds 1
    }