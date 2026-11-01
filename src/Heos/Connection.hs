module Heos.Connection (
  splitLines,
  withReconnect,
  Connection,
  subscribe,
  sendCommand,
  connectionParams,
  getPid,
  getAddr,
  ConnectionParams (..),
  withAvr,
) where

import Heos.Connection.Avr
import Heos.Connection.Framing (splitLines)
import Heos.Connection.Internal (
  Connection,
  connectionParams,
  getAddr,
  getPid,
  sendCommand,
  subscribe,
 )
import Heos.Connection.Parameters (ConnectionParams (..))
import Heos.Connection.Reconnect (
  withReconnect,
 )
