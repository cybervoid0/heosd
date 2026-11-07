-- | Подделка аппарата (этап 28): сценарии гоняются через эффект без сети.
-- Всё чисто: `runPureEff`, ни одного сокета.
module Heos.FakeSpec (spec) where

import Control.Monad (replicateM_)
import Data.Text (Text)
import Effectful (Eff, runPureEff, (:>))
import Heos.Effect.Heos
import Heos.Handler.Fake (runHeosFake)
import Heos.Types
import Test.Hspec

-- | Запустить сценарий против подделки. Результат чистый.
run :: Eff '[Heos] a -> a
run = runPureEff . runHeosFake

tidal :: SourceId
tidal = SourceId 10

-- | Что сейчас играет, по названию песни; тишина это пустое имя,
-- в сценариях здесь всегда что-то играет.
playing :: (Heos :> es) => Eff es Text
playing = maybe "" (.song) <$> getNowPlaying

-- | Цель первого найденного: всё найденное можно играть.
firstTarget :: (Heos :> es) => SearchKind -> Text -> Eff es Playable
firstTarget kind query =
  search tidal kind query >>= \case
    f : _ -> pure f.target
    [] -> error ("nothing found for " <> show query)

spec :: Spec
spec = do
  describe "static answers" $ do
    it "has one player, the MODEL 60n" $
      map (.pid) (run getPlayers) `shouldBe` [PlayerId (-1245882071)]

    it "has Tidal available and TuneIn not" $ do
      let availability = [(s.name, s.available) | s <- run getSources, s.name `elem` ["Tidal", "TuneIn"]]
      availability `shouldBe` [("TuneIn", False), ("Tidal", True)]

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

    -- Mute is its own switch on the device: the level stays what it was,
    -- and get_volume keeps reporting it.
    it "mute does not touch the volume" $
      run (setVolume 42 >> mute >> getVolume) `shouldBe` 42

    it "mute twice is back where it started" $
      run (mute >> mute >> getVolume) `shouldBe` 10

  describe "play state" $ do
    it "remembers what was set" $
      run (play >> getPlayState) `shouldBe` Play

    it "pause and stop are told apart" $
      run ((,) <$> (play >> pause >> getPlayState) <*> (stop >> getPlayState)) `shouldBe` (Pause, Stop)

  -- The fake starts with ten tracks of two albums; showQueue shows them
  -- as the device would, item by item, with the queue positions.
  describe "queue" $ do
    it "showQueue lists the whole queue in order" $ do
      let items = run showQueue
      length items `shouldBe` 10
      map (.song) (take 3 items) `shouldBe` ["Heaven's Gonna Burn Your Eyes", "Facing East", "The Outernationalist"]

    it "showQueue carries the ids the device uses to address an item" $
      [(i.qid, i.mid, i.albumId) | i <- take 1 (run showQueue)] `shouldBe` [(QueueId 1, MediaId "496328267", "496328266")]

    it "showQueue follows what was enqueued" $
      map (.song) (run (firstTarget TrackKind "blonde" >>= playItem >> showQueue)) `shouldBe` ["Lebanese Blonde"]

    it "clearQueue leaves nothing to show, nothing playing, and stops" $
      run (play >> clearQueue >> (,,) <$> showQueue <*> getNowPlaying <*> getPlayState) `shouldBe` ([], Nothing, Stop)

  describe "walking the queue" $ do
    it "playNext moves to the second track" $
      run (next >> playing) `shouldBe` "Facing East"

    it "playPrevious at the start stays on the first track" $
      run (prev >> playing) `shouldBe` "Heaven's Gonna Burn Your Eyes"

    it "playNext past the end stays on the last track" $
      run (replicateM_ 20 next >> playing) `shouldBe` "Air Batucada"

    it "playPrevious after playNext comes back" $
      run (next >> next >> prev >> playing) `shouldBe` "Facing East"

  describe "search" $ do
    -- The fake searches Tidal by track, album and artist (artists come back
    -- empty, as on the device: they have no target), and has no playlists.
    it "getSearchable names Tidal for tracks, albums and artists" $
      map (run . getSearchable) [TrackKind, AlbumKind, ArtistKind] `shouldBe` [[tidal], [tidal], [tidal]]

    it "getSearchable names no source for playlists" $
      run (getSearchable PlaylistKind) `shouldBe` []

    it "finds a track by part of its name, case-insensitively" $ do
      let found = run (search tidal TrackKind "BLONDE")
      map (.name) found `shouldBe` ["Lebanese Blonde"]
      map (.kind) found `shouldBe` ["song"]
      map (.target) found `shouldBe` [PlayTrack tidal (ContainerId "LIBALBUM-BLONDE") (MediaId "496097054")]

    it "finds an album once, not once per track" $ do
      let found = run (search tidal AlbumKind "mirror")
      map (.name) found `shouldBe` ["Mirror Conspiracy"]
      map (.target) found `shouldBe` [PlayContainer tidal (ContainerId "496097050")]

    -- Every Found can be played, so an artist, which cannot, is not a Found.
    it "finds no artists: they have nothing to play" $
      run (search tidal ArtistKind "thievery") `shouldBe` []

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
            next
            b <- playing
            replicateM_ 10 next
            c <- playing
            pure [a, b, c]
      names `shouldBe` ["Treasures", "Lebanese Blonde", "Air Batucada"]

    it "AddToEnd leaves the current track alone" $
      run (firstTarget TrackKind "blonde" >>= enqueue AddToEnd >> playing) `shouldBe` "Heaven's Gonna Burn Your Eyes"

    it "AddPlayNext puts the track right after the current one" $
      run (firstTarget TrackKind "blonde" >>= enqueue AddPlayNext >> next >> playing) `shouldBe` "Lebanese Blonde"

    it "AddPlayNow puts the track in place of the current one" $
      run (firstTarget TrackKind "omid" >>= enqueue AddPlayNow >> playing) `shouldBe` "Omid"

    it "playAll plays the first and queues the rest after it" $ do
      let names = run $ do
            blonde <- firstTarget TrackKind "blonde"
            richest <- firstTarget AlbumKind "richest"
            playAll [blonde, richest]
            a <- playing
            next
            b <- playing
            pure [a, b]
      names `shouldBe` ["Lebanese Blonde", "Heaven's Gonna Burn Your Eyes"]

    it "playItem starts playback" $
      run (firstTarget TrackKind "blonde" >>= playItem >> getPlayState) `shouldBe` Play

    it "playing an unknown track empties the queue: nothing plays" $
      run (playItem (PlayTrack tidal (ContainerId "x") (MediaId "no-such")) >> getNowPlaying) `shouldBe` Nothing
