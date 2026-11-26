module Heos.Handler.AvrLive where

import Effectful (Eff, IOE, MonadIO (liftIO), (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Exception (throwIO)
import Heos.Command.Avr (AvrCommand (..))
import Heos.Connection.Avr
import Heos.Effect.Avr
import Heos.Error (AvrError (AvrUnavailable))

runAvrLive :: (IOE :> es) => Maybe AvrConnection -> Eff (Avr : es) a -> Eff es a
runAvrLive Nothing = interpret_ $ \_ -> throwIO AvrUnavailable
runAvrLive (Just conn) =
  interpret_ $
    liftIO . sendAvrCommand conn . \case
      EGetPower -> GetPower
      ESetPower pw -> SetPower pw
