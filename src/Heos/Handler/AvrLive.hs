module Heos.Handler.AvrLive where

import Effectful (Eff, IOE, MonadIO (liftIO), (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Heos.Command.Avr (AvrCommand (..))
import Heos.Connection.Avr
import Heos.Effect.Avr

runAvrLive :: (IOE :> es) => AvrConnection -> Eff (Avr : es) a -> Eff es a
runAvrLive conn =
  interpret_ $
    liftIO . sendAvrCommand conn . \case
      EGetPower -> GetPower
      ESetPower pw -> SetPower pw
