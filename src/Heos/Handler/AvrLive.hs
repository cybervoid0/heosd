module Heos.Handler.AvrLive where

import Effectful (Eff, IOE, MonadIO (liftIO), (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Heos.Command.Avr (AvrCommand (..))
import Heos.Connection.Avr
import Heos.Device (Device (..))
import Heos.Effect.Avr

runAvrLive :: (IOE :> es) => Device -> Eff (Avr : es) a -> Eff es a
runAvrLive dev =
  interpret_ $
    liftIO . sendAvrCommand dev.avr . \case
      EGetPower -> GetPower
      ESetPower pw -> SetPower pw
