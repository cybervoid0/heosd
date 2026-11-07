module Heos.AvrFakeSpec (spec) where

import Effectful (runPureEff)
import Heos.Effect.Avr (getPower, turnOff, turnOn)
import Heos.Effect.Heos (getPlayState, play)
import Heos.Handler.AvrFake (runAvrFake)
import Heos.Handler.Fake (runHeosFake)
import Heos.Types (OnOff (..), PlayState (..))
import Test.Hspec

-- The fake amplifier is a single cell: the power state. No network, no
-- time, so scenarios touching power run under runPureEff.

spec :: Spec
spec = describe "runAvrFake" $ do
  it "starts in standby" $
    runPureEff (runAvrFake getPower) `shouldBe` Off

  it "remembers the state it was switched to" $ do
    runPureEff (runAvrFake (turnOn >> getPower)) `shouldBe` On
    runPureEff (runAvrFake (turnOn >> turnOff >> getPower)) `shouldBe` Off

  it "keeps the state to one run" $ do
    runPureEff (runAvrFake turnOn) `shouldBe` ()
    runPureEff (runAvrFake getPower) `shouldBe` Off

  -- Both fakes stack like the live handlers do: a scenario that wakes the
  -- amplifier and starts playback is pure.
  it "stacks with the HEOS fake" $
    runPureEff (runAvrFake (runHeosFake (turnOn >> play >> (,) <$> getPower <*> getPlayState)))
      `shouldBe` (On, Play)
