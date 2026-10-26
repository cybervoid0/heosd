-- | Подделка аппарата (этап 28): сценарии гоняются через эффект без сети.
-- Всё чисто: `runPureEff`, ни одного сокета.
module Heos.FakeSpec (spec) where

import Control.Exception (evaluate)
import Control.Monad (replicateM_)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Effectful (Eff, runPureEff, (:>))
import Heos.Effect
import Heos.Handler.Fake (runHeosFake)
import Heos.Types
import Test.Hspec

-- | Запустить сценарий против подделки. Результат чистый.
run :: Eff '[Heos] a -> a
run = runPureEff . runHeosFake

tidal :: SourceId
tidal = SourceId 10

-- | Что сейчас играет, по названию песни.
playing :: (Heos :> es) => Eff es Text
playing = (.song) <$> getNowPlaying

-- | Первая играбельная цель из поиска.
firstTarget :: (Heos :> es) => SearchKind -> Text -> Eff es Playable
firstTarget kind query = do
  found <- search tidal kind query
  case mapMaybe (.target) found of
    t : _ -> pure t
    [] -> error ("nothing playable for " <> show query)

spec :: Spec
spec = do
  describe "static answers" $ do
    it "has one player, the MODEL 60n" $
      map (.pid) (run getPlayers) `shouldBe` [PlayerId (-1245882071)]

    it "has Tidal available and TuneIn not" $ do
      let availability = [(s.name, s.available) | s <- run getSources, s.name `elem` ["Tidal", "TuneIn"]]
      availability `shouldBe` [("TuneIn", TrueFalse False), ("Tidal", TrueFalse True)]

    it "starts on the first track of the queue, stopped" $
      run ((,) <$> playing <*> getPlayState) `shouldBe` ("Heaven's Gonna Burn Your Eyes", Stop)

  describe "volume" $ do
    it "starts at 10" $
      run getVolume `shouldBe` 10

    it "goes up and down by steps" $
      run (volumeUp 5 >> volumeDown 2 >> getVolume) `shouldBe` 13

    it "setVolume replaces" $
      run (setVolume 42 >> getVolume) `shouldBe` 42

    it "never exceeds 100" $
      run (setVolume 95 >> volumeUp 10 >> getVolume) `shouldBe` 100

    it "never drops below 0" $
      run (volumeDown 20 >> getVolume) `shouldBe` 0

  describe "play state" $ do
    it "remembers what was set" $
      run (setPlayState Play >> getPlayState) `shouldBe` Play

  describe "walking the queue" $ do
    it "playNext moves to the second track" $
      run (playNext >> playing) `shouldBe` "Facing East"

    it "playPrevious at the start stays on the first track" $
      run (playPrevious >> playing) `shouldBe` "Heaven's Gonna Burn Your Eyes"

    it "playNext past the end stays on the last track" $
      run (replicateM_ 20 playNext >> playing) `shouldBe` "Air Batucada"

    it "playPrevious after playNext comes back" $
      run (playNext >> playNext >> playPrevious >> playing) `shouldBe` "Facing East"

  describe "search" $ do
    it "finds a track by part of its name, case-insensitively" $ do
      let found = run (search tidal TrackKind "BLONDE")
      map (.name) found `shouldBe` ["Lebanese Blonde"]
      map (.kind) found `shouldBe` ["song"]
      map (.target) found `shouldBe` [Just (PlayTrack tidal (ContainerId "SEARCHED_TRACKS-BLONDE") (MediaId "496097054"))]

    it "finds an album once, not once per track" $ do
      let found = run (search tidal AlbumKind "mirror")
      map (.name) found `shouldBe` ["Mirror Conspiracy"]
      map (.target) found `shouldBe` [Just (PlayContainer tidal (ContainerId "496097050"))]

    it "finds an artist once, without a target" $ do
      let found = run (search tidal ArtistKind "thievery")
      map (.name) found `shouldBe` ["Thievery Corporation"]
      map (.target) found `shouldBe` [Nothing]

    it "does not match a song name when searching albums" $
      run (search tidal AlbumKind "blonde") `shouldBe` []

    it "has no playlists" $
      run (search tidal PlaylistKind "mirror") `shouldBe` []

  describe "find and play" $ do
    it "playItem on a track replaces the queue with it" $
      run (firstTarget TrackKind "blonde" >>= playItem >> playing) `shouldBe` "Lebanese Blonde"

    it "playItem on an album replaces the queue with its tracks, in order" $ do
      let names = run $ do
            firstTarget AlbumKind "mirror" >>= playItem
            a <- playing
            playNext
            b <- playing
            replicateM_ 10 playNext
            c <- playing
            pure [a, b, c]
      names `shouldBe` ["Treasures", "Lebanese Blonde", "Air Batucada"]

    it "AddToEnd leaves the current track alone" $
      run (firstTarget TrackKind "blonde" >>= enqueue AddToEnd >> playing) `shouldBe` "Heaven's Gonna Burn Your Eyes"

    it "AddPlayNext puts the track right after the current one" $
      run (firstTarget TrackKind "blonde" >>= enqueue AddPlayNext >> playNext >> playing) `shouldBe` "Lebanese Blonde"

    it "AddPlayNow puts the track in place of the current one" $
      run (firstTarget TrackKind "omid" >>= enqueue AddPlayNow >> playing) `shouldBe` "Omid"

    it "playAll plays the first and queues the rest after it" $ do
      let names = run $ do
            blonde <- firstTarget TrackKind "blonde"
            richest <- firstTarget AlbumKind "richest"
            playAll [blonde, richest]
            a <- playing
            playNext
            b <- playing
            pure [a, b]
      names `shouldBe` ["Lebanese Blonde", "Heaven's Gonna Burn Your Eyes"]

    it "playItem starts playback" $
      run (firstTarget TrackKind "blonde" >>= playItem >> getPlayState) `shouldBe` Play

    it "playing an unknown track empties the queue and getNowPlaying fails" $
      evaluate (run (playItem (PlayTrack tidal (ContainerId "x") (MediaId "no-such")) >> playing))
        `shouldThrow` \(ProtocolError _) -> True
