module Heos.EnvelopeSpec where

import Control.Monad (forM_)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Either (isRight)
import Heos.Envelope
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
  describe "Decoding JSON" $ do
    let dir = "test/fixtures/responses"
    files <- runIO $ listDirectory dir

    it (show (length files) <> " fixtures were found") $
      length files `shouldSatisfy` (> 10)

    forM_ files $ \file -> do
      it ("parses " <> file) $ do
        result <- eitherDecodeFileStrict @Envelope (dir </> file)
        result `shouldSatisfy` isRight

    it "heart_beat is parsed correctly" $ do
      r <- eitherDecodeFileStrict (dir </> "heart_beat.json")
      r `shouldBe` Right (Envelope (HeosHeader "system/heart_beat" Success "") Nothing Nothing)
