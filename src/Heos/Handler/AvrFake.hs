module Heos.Handler.AvrFake where

import Effectful (Eff)
import Effectful.Dispatch.Dynamic (reinterpret_)
import Effectful.State.Static.Local (evalState, gets, modify)
import Heos.Effect.Avr
import Heos.Types

newtype FakeAvrState = FakeAvrState
  { pwStatus :: OnOff
  }

defaultAvrFakeState :: FakeAvrState
defaultAvrFakeState =
  FakeAvrState
    { pwStatus = Off
    }

runAvrFake :: Eff (Avr : es) a -> Eff es a
runAvrFake = reinterpret_ (evalState defaultAvrFakeState) $ \case
  EGetPower -> gets @FakeAvrState (.pwStatus)
  ESetPower pw -> modify $ \s -> s{pwStatus = pw}