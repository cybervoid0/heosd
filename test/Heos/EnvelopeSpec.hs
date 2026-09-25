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
import Test.QuickCheck (property, (==>))

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

  describe "parseGroup" $ do
    it "parses every known group" $
      traverse parseGroup ["event", "system", "player", "browse", "group"]
        `shouldBe` Right [Event, System, Player, Browse, Group]

    it "rejects an unknown group, keeping its text" $
      parseGroup "foo" `shouldBe` Left (UnknownGroup "foo")

    it "is case-sensitive" $
      parseGroup "Event" `shouldBe` Left (UnknownGroup "Event")

    it "rejects an empty group" $
      parseGroup "" `shouldBe` Left (UnknownGroup "")

  describe "parseCommand" $ do
    it "splits group and name" $
      parseCommand "player/get_players" `shouldBe` Right (HeosCommand Player "get_players")

    it "rejects a command without a slash" $
      parseCommand "heart_beat" `shouldBe` Left (CommandFmtFailure "heart_beat")

    it "rejects a command with extra segments" $
      parseCommand "event/a/b" `shouldBe` Left (CommandFmtFailure "event/a/b")

    it "rejects an empty command" $
      parseCommand "" `shouldBe` Left (CommandFmtFailure "")

    it "reports an unknown group, not a format failure" $
      parseCommand "foo/bar" `shouldBe` Left (UnknownGroup "foo")

    it "rejects an empty name" $
      parseCommand "event/" `shouldBe` Left (CommandFmtFailure "event/")

    it "rejects an empty group" $
      parseCommand "/foo" `shouldBe` Left (CommandFmtFailure "/foo")

    it "rejects a lone slash" $
      parseCommand "/" `shouldBe` Left (CommandFmtFailure "/")

  describe "classifyEnvelope" $ do
    it "classifies event/* commands as events, carrying the name" $ do
      let env = envelope "event/player_volume_changed" (Just "pid=1&level=32")
      classifyEnvelope env `shouldBe` Right (IncomingEvent (HeosEvent "player_volume_changed") env)

    it "classifies 'command under process' as pending" $ do
      let env = envelope "browse/search" (Just "command under process&sid=10")
      classifyEnvelope env `shouldBe` Right (IncomingPending env)

    it "finds 'command under process' anywhere in the message" $ do
      let env = envelope "browse/search" (Just "sid=10&command under process")
      classifyEnvelope env `shouldBe` Right (IncomingPending env)

    it "classifies other commands as responses" $ do
      let env = envelope "player/get_players" (Just "")
      classifyEnvelope env `shouldBe` Right (IncomingResponse env)

    it "treats a missing message as a response" $ do
      let env = envelope "system/heart_beat" Nothing
      classifyEnvelope env `shouldBe` Right (IncomingResponse env)

    it "classifies group/* commands as responses" $ do
      let env = envelope "group/get_groups" Nothing
      classifyEnvelope env `shouldBe` Right (IncomingResponse env)

    it "prefers event over pending" $ do
      let env = envelope "event/whatever" (Just "command under process")
      classifyEnvelope env `shouldBe` Right (IncomingEvent (HeosEvent "whatever") env)

    it "fails on an unknown group" $
      classifyEnvelope (envelope "Event/foo" Nothing) `shouldBe` Left (UnknownGroup "Event")

    it "fails on an unknown group even when the message says pending" $
      classifyEnvelope (envelope "foo/bar" (Just "command under process")) `shouldBe` Left (UnknownGroup "foo")

    it "fails on a malformed command" $
      classifyEnvelope (envelope "player/event/foo" Nothing) `shouldBe` Left (CommandFmtFailure "player/event/foo")

    it "fails on an event without a name" $
      classifyEnvelope (envelope "event/" Nothing) `shouldBe` Left (CommandFmtFailure "event/")

    it "event/<name> with a non-empty name and no slashes is an event with that name" $
      property $ \(name :: String) (msg :: Maybe String) ->
        not (null name) && '/' `notElem` name ==>
          let env = envelope ("event/" <> T.pack name) (T.pack <$> msg)
           in classifyEnvelope env == Right (IncomingEvent (HeosEvent (T.pack name)) env)

    describe "on fixtures" $ do
      let dir = "test/fixtures/responses"
          expectations =
            [ ("event_player.json", Just "players_changed")
            , ("event_volume.json", Just "player_volume_changed")
            ]
      forM_ expectations $ \(file, expected) ->
        it (file <> " is the event " <> show expected) $ do
          decoded <- eitherDecodeFileStrict @Envelope (dir </> file)
          either expectationFailure (\env -> eventName <$> classifyEnvelope env `shouldBe` Right expected) decoded

      it "search_ok_pending.json is pending" $ do
        decoded <- eitherDecodeFileStrict @Envelope (dir </> "search_ok_pending.json")
        either expectationFailure (\env -> classifyEnvelope env `shouldBe` Right (IncomingPending env)) decoded

      files <- runIO $ listDirectory dir
      forM_ files $ \file ->
        it (file <> " classifies without error") $ do
          decoded <- eitherDecodeFileStrict @Envelope (dir </> file)
          either expectationFailure (\env -> classifyEnvelope env `shouldSatisfy` isRight) decoded

envelope :: Text -> Maybe Text -> Envelope
envelope cmd msg = Envelope (HeosHeader cmd (Just Success) msg) Nothing Nothing

eventName :: IncomingEnvelope -> Maybe Text
eventName = \case
  IncomingEvent ev _ -> Just ev.name
  _ -> Nothing
