module Heos.EnvelopeSpec where

import Control.Monad (forM_)
import Data.Aeson (eitherDecode, eitherDecodeFileStrict)
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import Data.Text qualified as T
import Heos.Envelope
import Heos.Url (UrlParam (..), decodeQuery, lookupParam, makePair)
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

    -- Events carry no "result" at all, so the field has to stay optional.
    it "an event without result parses, with result = Nothing" $
      decodeHeader "{\"command\":\"event/players_changed\"}"
        `shouldBe` Right (HeosHeader "event/players_changed" Nothing Nothing)

    it "a missing message parses as Nothing" $
      decodeHeader "{\"command\":\"system/heart_beat\",\"result\":\"success\"}"
        `shouldBe` Right (HeosHeader "system/heart_beat" (Just Success) Nothing)

    it "a null message parses as Nothing" $
      decodeHeader "{\"command\":\"system/heart_beat\",\"result\":\"success\",\"message\":null}"
        `shouldBe` Right (HeosHeader "system/heart_beat" (Just Success) Nothing)

    it "the raw message is kept as is, even on fail" $
      decodeHeader "{\"command\":\"x/y\",\"result\":\"fail\",\"message\":\"eid=2&text=ID Not Valid\"}"
        `shouldBe` Right (HeosHeader "x/y" (Just Fail) (Just "eid=2&text=ID Not Valid"))

    it "an unknown result value is a parse error" $
      decodeHeader "{\"command\":\"x/y\",\"result\":\"maybe\"}" `shouldSatisfy` isLeft

    it "a missing command is a parse error" $
      decodeHeader "{\"result\":\"success\"}" `shouldSatisfy` isLeft

  describe "classifyEnvelope" $ do
    describe "events" $ do
      it "classifies event/* as an event, carrying the name and the decoded message" $ do
        let env = envelope "event/player_volume_changed" (Just "pid=1&level=32")
        classifyEnvelope env
          `shouldBe` event "player_volume_changed" [pair "pid" "1", pair "level" "32"] env

      -- The event's parameters are what subscribers read: this is the real
      -- message the device sent when the volume knob was turned.
      it "lets a subscriber look up pid and level in the event" $ do
        let env = envelope "event/player_volume_changed" (Just "pid=-1245882071&level=21&mute=off")
        case classifyEnvelope env of
          IncomingEvent ev _ -> do
            lookupParam "pid" ev.params `shouldBe` Just "-1245882071"
            lookupParam "level" ev.params `shouldBe` Just "21"
            lookupParam "mute" ev.params `shouldBe` Just "off"
          other -> expectationFailure ("expected an event, got " <> show other)

      -- System events (players_changed, sources_changed) carry no message.
      it "an event without a message has no parameters" $ do
        let env = envelope "event/players_changed" Nothing
        classifyEnvelope env `shouldBe` event "players_changed" [] env

      it "keeps everything after event/ as the name, slashes included" $ do
        let env = envelope "event/a/b" Nothing
        classifyEnvelope env `shouldBe` event "a/b" [] env

      it "accepts an empty event name" $ do
        let env = envelope "event/" Nothing
        classifyEnvelope env `shouldBe` event "" [] env

      -- The flag is not swallowed: it stays in the parameters as a flag.
      it "prefers event over pending" $ do
        let env = envelope "event/whatever" (Just "command under process")
        classifyEnvelope env `shouldBe` event "whatever" [UrlFlag "command under process"] env

      it "prefers event over a failed result" $ do
        let env = envelopeWith (Just Fail) "event/whatever" (Just "eid=2&text=x")
        classifyEnvelope env `shouldBe` event "whatever" [pair "eid" "2", pair "text" "x"] env

      it "requires the event/ prefix at the start" $ do
        let env = envelope "player/event/foo" Nothing
        classifyEnvelope env `shouldBe` IncomingResponse Succeeded env

      it "is case-sensitive about the event/ prefix" $ do
        let env = envelope "Event/foo" Nothing
        classifyEnvelope env `shouldBe` IncomingResponse Succeeded env

      it "requires the slash after event" $ do
        let env = envelope "eventually/foo" Nothing
        classifyEnvelope env `shouldBe` IncomingResponse Succeeded env

      it "event/<suffix> is an event named <suffix>, whatever the message" $
        property $ \(suffix :: String) (msg :: Maybe String) ->
          let env = envelope ("event/" <> T.pack suffix) (T.pack <$> msg)
              params = maybe [] (decodeQuery . T.pack) msg
           in classifyEnvelope env == event (T.pack suffix) params env

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
        classifyEnvelope env `shouldBe` IncomingResponse Succeeded env

      it "does not match the text as a key of a pair" $ do
        let env = envelope "browse/search" (Just "command under process=1")
        classifyEnvelope env `shouldBe` IncomingResponse Succeeded env

      it "does not match the text as a value of a pair" $ do
        let env = envelope "browse/search" (Just "status=command under process")
        classifyEnvelope env `shouldBe` IncomingResponse Succeeded env

      it "is case-sensitive" $ do
        let env = envelope "browse/search" (Just "Command Under Process")
        classifyEnvelope env `shouldBe` IncomingResponse Succeeded env

      it "works for any command group, even an unknown one" $ do
        let env = envelope "foo/bar" (Just "command under process")
        classifyEnvelope env `shouldBe` IncomingPending env

    describe "responses" $ do
      it "classifies an ordinary command as a response" $ do
        let env = envelope "player/get_players" (Just "")
        classifyEnvelope env `shouldBe` IncomingResponse Succeeded env

      it "treats a missing message as a response" $ do
        let env = envelope "system/heart_beat" Nothing
        classifyEnvelope env `shouldBe` IncomingResponse Succeeded env

      it "treats an unknown command group as a response" $ do
        let env = envelope "foo/bar" (Just "pid=1")
        classifyEnvelope env `shouldBe` IncomingResponse Succeeded env

      it "treats a malformed command as a response" $ do
        let env = envelope "heart_beat" Nothing
        classifyEnvelope env `shouldBe` IncomingResponse Succeeded env

      -- Current rule: only an explicit fail is a failure. By the protocol a
      -- response always has a result, so a missing one is taken as success.
      it "treats a response without result as succeeded" $ do
        let env = envelopeWith Nothing "player/get_players" (Just "")
        classifyEnvelope env `shouldBe` IncomingResponse Succeeded env

    describe "response outcome" $ do
      it "success does not need eid or text in the message" $ do
        let env = envelopeWith (Just Success) "player/get_play_state" (Just "pid=1&state=play")
        classifyEnvelope env `shouldBe` IncomingResponse Succeeded env

      it "success ignores eid and text if they happen to be there" $ do
        let env = envelopeWith (Just Success) "x/y" (Just "eid=2&text=Oops")
        classifyEnvelope env `shouldBe` IncomingResponse Succeeded env

      it "fail extracts eid and text" $
        outcomeOfFail (Just "eid=2&text=ID Not Valid&pid=9")
          `shouldBe` Failed (HeosError (Just 2) (Just "ID Not Valid"))

      it "fail finds eid and text in any order" $
        outcomeOfFail (Just "pid=9&text=Oops&eid=13")
          `shouldBe` Failed (HeosError (Just 13) (Just "Oops"))

      it "fail unescapes the text" $
        outcomeOfFail (Just "eid=1&text=A%26B%3DC")
          `shouldBe` Failed (HeosError (Just 1) (Just "A&B=C"))

      it "fail without eid is still Failed" $
        outcomeOfFail (Just "text=Oops") `shouldBe` Failed (HeosError Nothing (Just "Oops"))

      it "fail with a non-numeric eid is still Failed, without eid" $
        outcomeOfFail (Just "eid=abc&text=Oops") `shouldBe` Failed (HeosError Nothing (Just "Oops"))

      it "fail with eid as a bare flag has no eid" $
        outcomeOfFail (Just "eid&text=Oops") `shouldBe` Failed (HeosError Nothing (Just "Oops"))

      it "fail without text is still Failed" $
        outcomeOfFail (Just "eid=2") `shouldBe` Failed (HeosError (Just 2) Nothing)

      it "fail with an empty message is still Failed" $
        outcomeOfFail (Just "") `shouldBe` Failed (HeosError Nothing Nothing)

      it "fail without a message is still Failed" $
        outcomeOfFail Nothing `shouldBe` Failed (HeosError Nothing Nothing)

      it "pending wins over fail" $ do
        let env = envelopeWith (Just Fail) "browse/search" (Just "command under process&eid=2")
        classifyEnvelope env `shouldBe` IncomingPending env

    describe "on fixtures" $ do
      let dir = "test/fixtures/responses"
          expected = \case
            "event_player.json" -> "event players_changed"
            "event_volume.json" -> "event player_volume_changed"
            "search_ok_pending.json" -> "pending"
            "error_bad_pid.json" -> "failed eid=Just 2 text=Just \"ID Not Valid\""
            _ -> "succeeded"
      files <- runIO $ listDirectory dir
      forM_ files $ \file ->
        it (file <> " is " <> expected file) $ do
          decoded <- eitherDecodeFileStrict @Envelope (dir </> file)
          either expectationFailure (\env -> describeKind (classifyEnvelope env) `shouldBe` expected file) decoded
 where
  decodeHeader = eitherDecode @HeosHeader
  -- The outcome a failed response gets, for a given message.
  outcomeOfFail msg = case classifyEnvelope (envelopeWith (Just Fail) "player/get_play_state" msg) of
    IncomingResponse outcome _ -> outcome
    other -> error ("expected a response, got " <> show other)

envelope :: Text -> Maybe Text -> Envelope
envelope = envelopeWith (Just Success)

event :: Text -> [UrlParam] -> Envelope -> IncomingEnvelope
event name params = IncomingEvent (HeosEvent name params)

pair :: Text -> Text -> UrlParam
pair k v = UrlKeyValue (makePair k v)

envelopeWith :: Maybe HeosResult -> Text -> Maybe Text -> Envelope
envelopeWith res cmd msg = Envelope (HeosHeader cmd res msg) Nothing Nothing

-- A compact description for the fixture table, so the table stays readable.
describeKind :: IncomingEnvelope -> String
describeKind = \case
  IncomingEvent (HeosEvent name _) _ -> "event " <> T.unpack name
  IncomingPending _ -> "pending"
  IncomingResponse Succeeded _ -> "succeeded"
  IncomingResponse (Failed e) _ -> "failed eid=" <> show e.eid <> " text=" <> show e.text
