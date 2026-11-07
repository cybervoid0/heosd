-- | Decoding the payloads the device sends into the records of 'Heos.Player'
--   and 'Heos.Media'. The fixtures are real replies from the MODEL 60n, so
--   these tests pin what the device actually sends, not what the spec says.
module Heos.PayloadSpec (spec) where

import Data.Aeson (FromJSON, Value, eitherDecodeFileStrict, eitherDecodeStrict, parseJSON)
import Data.Aeson.Types (parseEither)
import Data.ByteString (ByteString)
import Data.Either (isLeft)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Heos.Envelope.Heos (Envelope (..))
import Heos.Types
import System.FilePath ((</>))
import Test.Hspec

-- The payload of a recorded reply, or a failure if the file has none.
payloadOf :: FilePath -> IO Value
payloadOf file = do
  env <- eitherDecodeFileStrict @Envelope ("test/fixtures/responses" </> file)
  case env of
    Left err -> fail ("fixture " <> file <> ": " <> err)
    Right e -> maybe (fail ("fixture " <> file <> " has no payload")) pure e.payload

-- Decode a payload into the record under test; a parse failure fails the test
-- with aeson's message, which names the missing or mistyped key.
decodeAs :: (FromJSON a) => Value -> IO a
decodeAs = either fail pure . parseEither parseJSON

-- One search result written by hand, for the corner cases the fixture lacks.
decodeResult :: ByteString -> IO SearchResult
decodeResult = either fail pure . eitherDecodeStrict

-- The source with this name, or a failure naming what's missing.
named :: Text -> [MusicSource] -> IO MusicSource
named n = maybe (fail ("no source named " <> T.unpack n)) pure . find ((== n) . (.name))

spec :: Spec
spec = do
  describe "Player" $ do
    it "decodes the one player of get_players" $ do
      players <- payloadOf "get_players.json" >>= decodeAs @[Player]
      map (.name) players `shouldBe` ["Marantz MODEL 60n"]
      map (.pid) players `shouldBe` [PlayerId (-1245882071)]
      map (.network) players `shouldBe` [WiFi]

  describe "MusicSource" $ do
    it "decodes every source of get_music_sources" $ do
      sources <- payloadOf "get_music_sources.json" >>= decodeAs @[MusicSource]
      map (.name) sources
        `shouldBe` ["TuneIn", "Deezer", "Tidal", "SoundCloud", "Amazon", "Qobuz", "Local Music", "Playlists", "History", "AUX Input", "Favorites"]

    -- The device writes availability as the strings "true"/"false", and
    -- names the account only on services you are signed in to.
    it "reads availability and the optional account name" $ do
      sources <- payloadOf "get_music_sources.json" >>= decodeAs @[MusicSource]
      tidal <- named "Tidal" sources
      tunein <- named "TuneIn" sources
      tidal.sid `shouldBe` SourceId 10
      tidal.available `shouldBe` True
      tidal.serviceUsername `shouldBe` Just "ales.very@gmail.com"
      tunein.available `shouldBe` False
      tunein.serviceUsername `shouldBe` Nothing

  describe "Track" $ do
    it "decodes a Tidal station from get_now_playing_media" $ do
      np <- payloadOf "now_playing.json" >>= decodeAs @Track
      np.trackType `shouldBe` "station"
      np.song `shouldBe` "Lebanese Blonde"
      np.station `shouldBe` Just "Lebanese Blonde"
      np.artist `shouldBe` "Thievery Corporation"
      np.mid `shouldBe` MediaId "1"
      np.qid `shouldBe` QueueId 1

  describe "SearchResult" $ do
    it "decodes all fifty songs of a browse/search reply" $ do
      found <- payloadOf "search_ok_computer.json" >>= decodeAs @[SearchResult]
      length found `shouldBe` 50
      map (.resultType) found `shouldSatisfy` all (== "song")

    it "reads a song: mid and album present, cid absent" $ do
      found <- payloadOf "search_ok_computer.json" >>= decodeAs @[SearchResult]
      first <- case found of
        r : _ -> pure r
        [] -> fail "no search results"
      first.name `shouldBe` "ok computer"
      first.container `shouldBe` False
      first.playable `shouldBe` True
      first.mid `shouldBe` Just (MediaId "443242295")
      first.cid `shouldBe` Nothing
      first.artist `shouldBe` Just "Sef"
      first.album `shouldBe` Just "lieve monsters"

    -- A container (album, playlist) has a cid and no mid, and may lack the
    -- song-only fields. Not in the recorded replies yet, so hand-written
    -- from the spec's shape.
    it "reads a container: cid present, song-only fields absent" $ do
      let json = "{\"container\": \"yes\", \"cid\": \"77\", \"type\": \"album\", \"playable\": \"yes\", \"name\": \"OK Computer\", \"image_url\": \"x\", \"artist\": \"Radiohead\"}"
      r <- either fail pure (eitherDecodeStrict @SearchResult json)
      r.container `shouldBe` True
      r.cid `shouldBe` Just (ContainerId "77")
      r.mid `shouldBe` Nothing
      r.album `shouldBe` Nothing
      r.albumId `shouldBe` Nothing

  -- A queue item is thinner than now_playing_media: no type, no sid, no
  -- station. The fixture is the recorded queue of 75 Tidal tracks.
  describe "QueueItem" $ do
    it "decodes every item of the recorded queue" $ do
      items <- payloadOf "get_queue.json" >>= decodeAs @[QueueItem]
      length items `shouldBe` 75
      map (.qid) (take 3 items) `shouldBe` map QueueId [1, 2, 3]

    it "keeps what the device sends: song, album, artist, ids" $ do
      items <- payloadOf "get_queue.json" >>= decodeAs @[QueueItem]
      case items of
        i : _ -> do
          i.song `shouldBe` "Heaven's Gonna Burn Your Eyes"
          i.album `shouldBe` "The Richest Man In Babylon"
          i.artist `shouldBe` "Thievery Corporation"
          i.mid `shouldBe` MediaId "496328267"
          i.albumId `shouldBe` "496328266"
        [] -> expectationFailure "empty queue"

  -- The criteria are per source: the device says which scid means what.
  describe "criterionFor" $ do
    it "finds the scid of each kind in the Tidal criteria reply" $ do
      crits <- payloadOf "search_criteria.json" >>= decodeAs @[SearchCriterion]
      map (`criterionFor` crits) [ArtistKind, AlbumKind, TrackKind, PlaylistKind]
        `shouldBe` map (Just . SearchCriteriaId) [1, 2, 3, 6]

    it "finds nothing when the source offers no criteria" $
      criterionFor TrackKind [] `shouldBe` Nothing

  -- From what the device sent to what the scenario can play. A song is
  -- played out of its album container, LIBALBUM-<album_id> on Tidal: the
  -- SEARCHED_TRACKS-<query> container of the spec the MODEL 60n rejects
  -- with "System error", and a bare album_id with "cannot play".
  describe "toFound" $ do
    it "turns every searched song into a track of its LIBALBUM container" $ do
      results <- payloadOf "search_ok_computer.json" >>= decodeAs @[SearchResult]
      let found = mapMaybe (toFound (SourceId 10)) results
      length found `shouldBe` 50
      map (.kind) found `shouldSatisfy` all (== "song")
      case found of
        f : _ -> do
          f.name `shouldBe` "ok computer"
          f.artist `shouldBe` Just "Sef"
          f.target `shouldBe` PlayTrack (SourceId 10) (ContainerId "LIBALBUM-443242286") (MediaId "443242295")
        [] -> expectationFailure "no results"

    it "turns a container into PlayContainer with its cid" $ do
      r <- decodeResult "{\"container\": \"yes\", \"cid\": \"LIBALBUM-77\", \"type\": \"album\", \"playable\": \"yes\", \"name\": \"OK Computer\", \"image_url\": \"x\"}"
      (.target) <$> toFound (SourceId 10) r `shouldBe` Just (PlayContainer (SourceId 10) (ContainerId "LIBALBUM-77"))

    it "drops an item the device marks unplayable" $ do
      r <- decodeResult "{\"container\": \"no\", \"mid\": \"5\", \"album_id\": \"7\", \"type\": \"song\", \"playable\": \"no\", \"name\": \"x\", \"image_url\": \"x\"}"
      toFound (SourceId 10) r `shouldBe` Nothing

    it "drops a song without a mid" $ do
      r <- decodeResult "{\"container\": \"no\", \"album_id\": \"7\", \"type\": \"song\", \"playable\": \"yes\", \"name\": \"x\", \"image_url\": \"x\"}"
      toFound (SourceId 10) r `shouldBe` Nothing

    it "drops a song without an album_id: there is no container to play it from" $ do
      r <- decodeResult "{\"container\": \"no\", \"mid\": \"5\", \"type\": \"song\", \"playable\": \"yes\", \"name\": \"x\", \"image_url\": \"x\"}"
      toFound (SourceId 10) r `shouldBe` Nothing

    it "drops a container without a cid" $ do
      r <- decodeResult "{\"container\": \"yes\", \"type\": \"album\", \"playable\": \"yes\", \"name\": \"x\", \"image_url\": \"x\"}"
      toFound (SourceId 10) r `shouldBe` Nothing

  -- Two spellings of yes in one protocol: search results say "yes"/"no",
  -- sources say "true"/"false". Each parser accepts only its own.
  describe "yesNo and trueFalse" $ do
    it "yesNo reads yes and no" $
      map (parseEither yesNo) ["yes", "no"] `shouldBe` [Right True, Right False]
    it "yesNo rejects true/false" $
      parseEither yesNo "true" `shouldSatisfy` isLeft
    it "trueFalse reads true and false" $
      map (parseEither trueFalse) ["true", "false"] `shouldBe` [Right True, Right False]
    it "trueFalse rejects yes/no" $
      parseEither trueFalse "yes" `shouldSatisfy` isLeft