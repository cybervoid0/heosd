module Heos.Connection (
  withReconnect,
  HeosConnection (..),
  HeosConnectionParams (..),
  AvrConnectionParams (..),
  subscribe,
  sendCommand,
  connectionParams,
  getAddr,
  withAvr,
) where

import Heos.Connection.Avr

import Heos.Connection.Heos (
  sendCommand,
  subscribe,
 )
import Heos.Connection.Reconnect (
  withReconnect,
 )
import Heos.Connection.Types (
  HeosConnection (..),
  HeosConnectionParams (..),
  connectionParams,
  getAddr,
 )