module Heos.Effect.Avr (
  Avr (..),
  getPower,
  setPower,
) where

import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (send)
import Heos.Types (OnOff)

data Avr :: Effect where
  EGetPower :: Avr m OnOff
  ESetPower :: OnOff -> Avr m ()

type instance DispatchOf Avr = Dynamic

getPower :: (Avr :> es) => Eff es OnOff
getPower = send EGetPower

setPower :: (Avr :> es) => OnOff -> Eff es ()
setPower = send . ESetPower