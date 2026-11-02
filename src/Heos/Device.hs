module Heos.Device where

import Heos.Connection (HeosConnection)
import Heos.Connection.Avr (AvrConnection)
import Heos.Device.Parameters
import Heos.Device.State

data Device = Device
  { heos :: HeosConnection
  , avr :: AvrConnection
  , state :: DeviceState
  }

withDevice :: DeviceParams -> (Device -> IO a) -> IO a
withDevice = undefined