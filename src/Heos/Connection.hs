module Heos.Connection (
  splitLines,
  withReconnect,
  Connection,
  subscribe,
  sendCommand,
  connectionParams,
  getPid,
  ConnectionParams (..),
) where

import Heos.Connection.Framing (splitLines)
import Heos.Connection.Internal (
  Connection,
  connectionParams,
  getPid,
  sendCommand,
  subscribe,
 )
import Heos.Connection.Parameters (ConnectionParams (..))
import Heos.Connection.Reconnect (
  withReconnect,
 )
