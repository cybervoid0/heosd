module Heos.EnvelopeSpec where

import Control.Monad (forM_)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Either (isRight)
import Data.Text (Text)
import Data.Text qualified as T
import Heos.Envelope
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Test.Hspec
import Test.QuickCheck (property)

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
      r `shouldBe` Right (Envelope (HeosHeader "system/heart_beat" (Just Success) (Just "")) Nothing Nothing)

  describe "classifyEnvelope" $ do
    it "classifies event/* commands as events" $
      kind (classifyEnvelope (envelope "event/player_volume_changed" (Just "pid=1&level=32"))) `shouldBe` "event"

    it "classifies 'command under process' as pending" $
      kind (classifyEnvelope (envelope "browse/search" (Just "command under process&sid=10"))) `shouldBe` "pending"

    it "finds 'command under process' anywhere in the message" $
      kind (classifyEnvelope (envelope "browse/search" (Just "sid=10&command under process"))) `shouldBe` "pending"

    it "classifies other commands as responses" $
      kind (classifyEnvelope (envelope "player/get_players" (Just ""))) `shouldBe` "response"

    it "treats a missing message as a response" $
      kind (classifyEnvelope (envelope "system/heart_beat" Nothing)) `shouldBe` "response"

    it "prefers event over pending" $
      kind (classifyEnvelope (envelope "event/whatever" (Just "command under process"))) `shouldBe` "event"

    it "requires the event/ prefix at the start" $
      kind (classifyEnvelope (envelope "player/event/foo" Nothing)) `shouldBe` "response"

    it "is case-sensitive about the event/ prefix" $
      kind (classifyEnvelope (envelope "Event/foo" Nothing)) `shouldBe` "response"

    it "requires the slash after event" $
      kind (classifyEnvelope (envelope "eventually/foo" Nothing)) `shouldBe` "response"

    it "any event/* command is an event, whatever the message" $
      property $ \(suffix :: String) (msg :: Maybe String) ->
        kind (classifyEnvelope (envelope ("event/" <> T.pack suffix) (T.pack <$> msg))) == "event"

    describe "on fixtures" $ do
      let dir = "test/fixtures/responses"
          expectations =
            [ ("event_player.json", "event")
            , ("event_volume.json", "event")
            , ("search_ok_pending.json", "pending")
            , ("search_ok_computer.json", "response")
            , ("heart_beat.json", "response")
            , ("error_bad_pid.json", "response")
            ]
      forM_ expectations $ \(file, expected) ->
        it (file <> " is " <> expected) $ do
          decoded <- eitherDecodeFileStrict @Envelope (dir </> file)
          either expectationFailure (\env -> kind (classifyEnvelope env) `shouldBe` expected) decoded

-- IncomingEnvelope has no Show/Eq, so compare by constructor name.
kind :: IncomingEnvelope -> String
kind = \case
  IncomingEvent _ -> "event"
  IncomingPending _ -> "pending"
  IncomingResponse _ -> "response"

envelope :: Text -> Maybe Text -> Envelope
envelope cmd msg = Envelope (HeosHeader cmd (Just Success) msg) Nothing Nothing
