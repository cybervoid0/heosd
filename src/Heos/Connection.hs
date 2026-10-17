module Heos.Connection (
  splitLines,
  withReconnect,
  Connection,
  subscribe,
  sendCommand,
) where

import Heos.Connection.Internal (
  Connection,
  sendCommand,
  splitLines,
  subscribe,
 )
import Heos.Connection.Reconnect (
  withReconnect,
 )
