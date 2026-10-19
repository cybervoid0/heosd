-- | Knobs of a connection. Fixed values for now; the config (stage 33) will
--   fill them in. Kept as a record so tests and the config can pass their
--   own instead of the defaults.
module Heos.Connection.Parameters (ConnectionParams (..), defaultParams) where

import Heos.Player (PlayerId (PlayerId))
import Heos.Time (Seconds (..))

data ConnectionParams = ConnectionParams
  { commandTimeout :: Seconds
  -- ^ how long a command may wait for its reply
  , heartbeatDelay :: Seconds
  -- ^ pause between heartbeats on each socket
  , playerId :: PlayerId
  -- ^ the player the daemon controls
  }

defaultParams :: ConnectionParams
defaultParams =
  ConnectionParams
    { commandTimeout = Seconds 5
    , heartbeatDelay = Seconds 30
    , playerId = PlayerId (-1245882071)
    }
