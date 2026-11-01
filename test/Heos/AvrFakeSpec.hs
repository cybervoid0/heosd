module Heos.AvrFakeSpec (spec) where

import Effectful (runPureEff)
import Heos.Effect (getPlayState, getPower, setPlayState, setPower)
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
    runPureEff (runAvrFake (setPower On >> getPower)) `shouldBe` On
    runPureEff (runAvrFake (setPower On >> setPower Off >> getPower)) `shouldBe` Off

  it "keeps the state to one run" $ do
    runPureEff (runAvrFake (setPower On)) `shouldBe` ()
    runPureEff (runAvrFake getPower) `shouldBe` Off

  -- Both fakes stack like the live handlers do: a scenario that wakes the
  -- amplifier and starts playback is pure.
  it "stacks with the HEOS fake" $
    runPureEff (runAvrFake (runHeosFake (setPower On >> setPlayState Play >> (,) <$> getPower <*> getPlayState)))
      `shouldBe` (On, Play)
