module Heos.EnvelopeSpec where

import Control.Monad (forM_)
import Data.Aeson (eitherDecode, eitherDecodeFileStrict)
import Data.Either (isLeft, isRight)
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
      r `shouldBe` Right (Envelope (HeosHeader "system/heart_beat" (Just Succeeded) (Just "")) Nothing Nothing)

  describe "HeosHeader outcome" $ do
    it "success gives Succeeded" $
      outcomeOf "{\"command\":\"player/get_players\",\"result\":\"success\",\"message\":\"pid=1\"}"
        `shouldBe` Right (Just Succeeded)

    it "success does not need eid or text in the message" $
      outcomeOf "{\"command\":\"system/heart_beat\",\"result\":\"success\",\"message\":\"\"}"
        `shouldBe` Right (Just Succeeded)

    it "success without a message key still parses" $
      decodeHeader "{\"command\":\"system/heart_beat\",\"result\":\"success\"}"
        `shouldBe` Right (HeosHeader "system/heart_beat" (Just Succeeded) Nothing)

    it "no result (an event) gives Nothing" $
      outcomeOf "{\"command\":\"event/players_changed\"}"
        `shouldBe` Right Nothing

    it "fail extracts eid and text" $
      outcomeOf "{\"command\":\"player/get_play_state\",\"result\":\"fail\",\"message\":\"eid=2&text=ID Not Valid&pid=9\"}"
        `shouldBe` Right (Just (Failed (HeosError (Just 2) (Just "ID Not Valid"))))

    it "fail finds eid and text in any order" $
      outcomeOf "{\"command\":\"x/y\",\"result\":\"fail\",\"message\":\"pid=9&text=Oops&eid=13\"}"
        `shouldBe` Right (Just (Failed (HeosError (Just 13) (Just "Oops"))))

    it "fail unescapes the text" $
      outcomeOf "{\"command\":\"x/y\",\"result\":\"fail\",\"message\":\"eid=1&text=A%26B%3DC\"}"
        `shouldBe` Right (Just (Failed (HeosError (Just 1) (Just "A&B=C"))))

    it "fail without eid is still Failed" $
      outcomeOf "{\"command\":\"x/y\",\"result\":\"fail\",\"message\":\"text=Oops\"}"
        `shouldBe` Right (Just (Failed (HeosError Nothing (Just "Oops"))))

    it "fail with a non-numeric eid is still Failed, without eid" $
      outcomeOf "{\"command\":\"x/y\",\"result\":\"fail\",\"message\":\"eid=abc&text=Oops\"}"
        `shouldBe` Right (Just (Failed (HeosError Nothing (Just "Oops"))))

    it "fail with eid as a bare flag has no eid" $
      outcomeOf "{\"command\":\"x/y\",\"result\":\"fail\",\"message\":\"eid&text=Oops\"}"
        `shouldBe` Right (Just (Failed (HeosError Nothing (Just "Oops"))))

    it "fail without text is still Failed" $
      outcomeOf "{\"command\":\"x/y\",\"result\":\"fail\",\"message\":\"eid=2\"}"
        `shouldBe` Right (Just (Failed (HeosError (Just 2) Nothing)))

    it "fail with an empty message is still Failed" $
      outcomeOf "{\"command\":\"x/y\",\"result\":\"fail\",\"message\":\"\"}"
        `shouldBe` Right (Just (Failed (HeosError Nothing Nothing)))

    it "fail without a message key is still Failed" $
      outcomeOf "{\"command\":\"x/y\",\"result\":\"fail\"}"
        `shouldBe` Right (Just (Failed (HeosError Nothing Nothing)))

    it "fail with a null message is still Failed" $
      outcomeOf "{\"command\":\"x/y\",\"result\":\"fail\",\"message\":null}"
        `shouldBe` Right (Just (Failed (HeosError Nothing Nothing)))

    it "an unknown result value is a parse error" $
      outcomeOf "{\"command\":\"x/y\",\"result\":\"maybe\"}" `shouldSatisfy` isLeft

    it "a missing command is a parse error" $
      outcomeOf "{\"result\":\"success\"}" `shouldSatisfy` isLeft

    describe "on fixtures" $ do
      let dir = "test/fixtures/responses"
          expected = \case
            "error_bad_pid.json" -> Just (Failed (HeosError (Just 2) (Just "ID Not Valid")))
            "event_player.json" -> Nothing
            "event_volume.json" -> Nothing
            _ -> Just Succeeded
      files <- runIO $ listDirectory dir
      forM_ files $ \file ->
        it (file <> " has outcome " <> show (expected file)) $ do
          decoded <- eitherDecodeFileStrict @Envelope (dir </> file)
          (.heos.outcome) <$> decoded `shouldBe` Right (expected file)

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
      it "recognizes the flag at the start of the message" $ do
        let env = envelope "browse/search" (Just "command under process&sid=10")
        classifyEnvelope env `shouldBe` IncomingPending env

      it "recognizes the flag at the end of the message" $ do
        let env = envelope "browse/search" (Just "sid=10&command under process")
        classifyEnvelope env `shouldBe` IncomingPending env

      it "recognizes the flag in the middle of the message" $ do
        let env = envelope "browse/search" (Just "sid=10&command under process&scid=3")
        classifyEnvelope env `shouldBe` IncomingPending env

      it "recognizes the flag as the whole message" $ do
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
        let env = Envelope (HeosHeader "player/get_play_state" (Just (Failed (HeosError (Just 2) (Just "ID Not Valid")))) (Just "eid=2&text=ID Not Valid")) Nothing Nothing
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

 where
  decodeHeader = eitherDecode @HeosHeader
  outcomeOf json = (.outcome) <$> decodeHeader json

envelope :: Text -> Maybe Text -> Envelope
envelope cmd msg = Envelope (HeosHeader cmd (Just Succeeded) msg) Nothing Nothing

-- A compact description for the fixture table, so the table stays readable.
describeKind :: IncomingEnvelope -> String
describeKind = \case
  IncomingEvent name _ -> "event " <> T.unpack name
  IncomingPending _ -> "pending"
  IncomingResponse _ -> "response"
