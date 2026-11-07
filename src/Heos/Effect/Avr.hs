module Heos.Effect.Avr (
  Avr (..),
  getPower,
  turnOn,
  turnOff,
) where

import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (send)
import Heos.Types (OnOff (..))

data Avr :: Effect where
  EGetPower :: Avr m OnOff
  ESetPower :: OnOff -> Avr m ()

type instance DispatchOf Avr = Dynamic

getPower :: (Avr :> es) => Eff es OnOff
getPower = send EGetPower

-- private
setPower :: (Avr :> es) => OnOff -> Eff es ()
setPower = send . ESetPower

turnOn :: (Avr :> es) => Eff es ()
turnOn = setPower On

turnOff :: (Avr :> es) => Eff es ()
turnOff = setPower Off
