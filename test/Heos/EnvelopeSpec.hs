module Heos.EnvelopeSpec where

import Control.Monad (forM_)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Either (isRight)
import Data.Maybe (fromMaybe)
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
    describe "events" $ do
      it "classifies event/* as an event, carrying the name" $ do
        let env = envelope "event/player_volume_changed" (Just "pid=1&level=32")
        classifyEnvelope env `shouldBe` IncomingEvent "player_volume_changed" env

      it "keeps everything after event/ as the name, slashes included" $ do
        let env = envelope "event/a/b" Nothing
        classifyEnvelope env `shouldBe` IncomingEvent "a/b" env

      it "accepts an empty event name" $ do
        let env = envelope "event/" Nothing
        classifyEnvelope env `shouldBe` IncomingEvent "" env

      it "prefers event over pending" $ do
        let env = envelope "event/whatever" (Just "command under process")
        classifyEnvelope env `shouldBe` IncomingEvent "whatever" env

      it "requires the event/ prefix at the start" $ do
        let env = envelope "player/event/foo" Nothing
        classifyEnvelope env `shouldBe` IncomingResponse env

      it "is case-sensitive about the event/ prefix" $ do
        let env = envelope "Event/foo" Nothing
        classifyEnvelope env `shouldBe` IncomingResponse env

      it "requires the slash after event" $ do
        let env = envelope "eventually/foo" Nothing
        classifyEnvelope env `shouldBe` IncomingResponse env

      it "event/<suffix> is an event named <suffix>, whatever the message" $
        property $ \(suffix :: String) (msg :: Maybe String) ->
          let env = envelope ("event/" <> T.pack suffix) (T.pack <$> msg)
           in classifyEnvelope env == IncomingEvent (T.pack suffix) env

    describe "pending" $ do
      it "recognises the flag at the start of the message" $ do
        let env = envelope "browse/search" (Just "command under process&sid=10")
        classifyEnvelope env `shouldBe` IncomingPending env

      it "recognises the flag at the end of the message" $ do
        let env = envelope "browse/search" (Just "sid=10&command under process")
        classifyEnvelope env `shouldBe` IncomingPending env

      it "recognises the flag in the middle of the message" $ do
        let env = envelope "browse/search" (Just "sid=10&command under process&scid=3")
        classifyEnvelope env `shouldBe` IncomingPending env

      it "recognises the flag as the whole message" $ do
        let env = envelope "browse/search" (Just "command under process")
        classifyEnvelope env `shouldBe` IncomingPending env

      it "ignores stray separators around the flag" $ do
        let env = envelope "browse/search" (Just "&&command under process&")
        classifyEnvelope env `shouldBe` IncomingPending env

      it "does not match the text as part of a longer chunk" $ do
        let env = envelope "browse/search" (Just "command under processing&sid=10")
        classifyEnvelope env `shouldBe` IncomingResponse env

      it "does not match the text as a key of a pair" $ do
        let env = envelope "browse/search" (Just "command under process=1")
        classifyEnvelope env `shouldBe` IncomingResponse env

      it "does not match the text as a value of a pair" $ do
        let env = envelope "browse/search" (Just "status=command under process")
        classifyEnvelope env `shouldBe` IncomingResponse env

      it "is case-sensitive" $ do
        let env = envelope "browse/search" (Just "Command Under Process")
        classifyEnvelope env `shouldBe` IncomingResponse env

      it "works for any command group, even an unknown one" $ do
        let env = envelope "foo/bar" (Just "command under process")
        classifyEnvelope env `shouldBe` IncomingPending env

    describe "responses" $ do
      it "classifies an ordinary command as a response" $ do
        let env = envelope "player/get_players" (Just "")
        classifyEnvelope env `shouldBe` IncomingResponse env

      it "treats a missing message as a response" $ do
        let env = envelope "system/heart_beat" Nothing
        classifyEnvelope env `shouldBe` IncomingResponse env

      it "treats an unknown command group as a response" $ do
        let env = envelope "foo/bar" (Just "pid=1")
        classifyEnvelope env `shouldBe` IncomingResponse env

      it "treats a malformed command as a response" $ do
        let env = envelope "heart_beat" Nothing
        classifyEnvelope env `shouldBe` IncomingResponse env

      it "treats a failed result as a response" $ do
        let env = Envelope (HeosHeader "player/get_play_state" (Just Fail) (Just "eid=2&text=ID Not Valid")) Nothing Nothing
        classifyEnvelope env `shouldBe` IncomingResponse env

    describe "on fixtures" $ do
      let dir = "test/fixtures/responses"
          expected = \case
            "event_player.json" -> Just "event players_changed"
            "event_volume.json" -> Just "event player_volume_changed"
            "search_ok_pending.json" -> Just "pending"
            _ -> Nothing
      files <- runIO $ listDirectory dir
      forM_ files $ \file -> do
        let want = fromMaybe "response" (expected file)
        it (file <> " is " <> want) $ do
          decoded <- eitherDecodeFileStrict @Envelope (dir </> file)
          either expectationFailure (\env -> describeKind (classifyEnvelope env) `shouldBe` want) decoded

envelope :: Text -> Maybe Text -> Envelope
envelope cmd msg = Envelope (HeosHeader cmd (Just Success) msg) Nothing Nothing

-- A compact description for the fixture table, so the table stays readable.
describeKind :: IncomingEnvelope -> String
describeKind = \case
  IncomingEvent name _ -> "event " <> T.unpack name
  IncomingPending _ -> "pending"
  IncomingResponse _ -> "response"
