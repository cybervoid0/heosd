module Heos.Connection.Types where

import Control.Concurrent (MVar)
import Control.Concurrent.STM (TChan, TMVar, TVar)
import Control.Retry (RetryPolicyM)
import Data.Map (Map)
import Data.Text (Text)
import Heos.Envelope.Heos (Envelope, HeosEvent)
import Heos.Error (ConnectionError)
import Heos.Time (Seconds)
import Heos.Types (PlayerId)
import Network.Socket (HostAddress, PortNumber, Socket)

type EitherEnvelope = Either ConnectionError Envelope

-- | What a HEOS connection is opened with.
data HeosConnectionParams = HeosConnectionParams
  { commandTimeout :: Seconds
  -- ^ how long a command may wait for its reply
  , heartbeatDelay :: Seconds
  -- ^ pause between heartbeats on each socket
  , maxLineLength :: Int
  -- ^ how many bytes the device may send without a "\r\n" before the link
  , port :: PortNumber
  , retryPolicy :: RetryPolicyM IO
  }

-- | The two TCP sockets to the device, as the vendor recommends: one
--   carries notifications, the other carries commands, so a burst of
--   events never delays a reply.
data ConnectionSockets = ConnectionSockets
  { address :: HostAddress
  , events :: Socket
  , commands :: Socket
  }

getAddr :: HeosConnection -> HostAddress
getAddr conn = conn.sockets.address

data Waiting = Waiting
  { path :: Text
  , box :: TMVar EitherEnvelope
  }

-- | Everything the two sockets share. The cells are created once per
--   connection in 'runConnection'; both readers and every sender use them.
data ConnectionProps = ConnectionProps
  { events :: TChan HeosEvent
  -- ^ broadcast: the reader writes, subscribers read their own copies via 'subscribe'
  , waiting :: TVar (Map Int Waiting)
  -- ^ commands in flight, by SEQUENCE: where the reader puts each reply
  , nextSeq :: TVar Int
  -- ^ the one SEQUENCE counter for this connection
  , sendLock :: MVar ()
  -- ^ one command in flight at a time, across both sockets; see 'sendOn'
  , params :: HeosConnectionParams
  }

-- | A live link to one HEOS device: the sockets and what they share.
--   Handed to the user's action; abstract outside this module.
data HeosConnection = HeosConnection
  { pid :: PlayerId
  , sockets :: ConnectionSockets
  , props :: ConnectionProps
  }

-- | The parameters this connection was opened with.
connectionParams :: HeosConnection -> HeosConnectionParams
connectionParams conn = conn.props.params