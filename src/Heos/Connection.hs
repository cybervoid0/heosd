module Heos.Connection (
  splitLines,
  withReconnect,
  Connection,
  subscribe,
  sendCommand,
) where

import Heos.Connection.Framing (splitLines)
import Heos.Connection.Internal (
  Connection,
  sendCommand,
  subscribe,
 )
import Heos.Connection.Reconnect (
  withReconnect,
 )
