module Heos.Connection (
  withReconnect,
  HeosConnection,
  subscribe,
  sendCommand,
  connectionParams,
  getPid,
  getAddr,
  ConnectionParams (..),
  withAvr,
) where

import Heos.Connection.Avr

import Heos.Connection.Heos (
  HeosConnection,
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
