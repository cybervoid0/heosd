-- | Decoding the payloads the device sends into the records of 'Heos.Player'
--   and 'Heos.Media'. The fixtures are real replies from the MODEL 60n, so
--   these tests pin what the device actually sends, not what the spec says.
module Heos.PayloadSpec (spec) where

import Data.Aeson (FromJSON, Value (String), eitherDecodeFileStrict, eitherDecodeStrict, parseJSON)
import Data.Aeson.Types (parseEither)
import Data.Either (isLeft)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Heos.Envelope (Envelope (..))
import Heos.Media (ContainerId (..), Found (..), MediaId (..), MusicSource (..), Playable (..), SearchCriteriaId (..), SearchCriterion, SearchKind (..), SearchResult (..), SourceId (..), TrueFalse (..), YesNo (..), criterionFor, toFound)
import Heos.Player (Network (..), NowPlaying (..), Player (..), PlayerId (..), QueueId (..))
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
decodeAs :: FromJSON a => Value -> IO a
decodeAs = either fail pure . parseEither parseJSON

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
      tidal.available `shouldBe` TrueFalse True
      tidal.service_username `shouldBe` Just "ales.very@gmail.com"
      tunein.available `shouldBe` TrueFalse False
      tunein.service_username `shouldBe` Nothing

  describe "NowPlaying" $ do
    it "decodes a Tidal station from get_now_playing_media" $ do
      np <- payloadOf "now_playing.json" >>= decodeAs @NowPlaying
      np._type `shouldBe` "station"
      np.song `shouldBe` "Lebanese Blonde"
      np.station `shouldBe` Just "Lebanese Blonde"
      np.artist `shouldBe` "Thievery Corporation"
      np.mid `shouldBe` MediaId "1"
      np.qid `shouldBe` QueueId 1

  describe "SearchResult" $ do
    it "decodes all fifty songs of a browse/search reply" $ do
      found <- payloadOf "search_ok_computer.json" >>= decodeAs @[SearchResult]
      length found `shouldBe` 50
      map (._type) found `shouldSatisfy` all (== "song")

    it "reads a song: mid and album present, cid absent" $ do
      found <- payloadOf "search_ok_computer.json" >>= decodeAs @[SearchResult]
      first <- case found of
        r : _ -> pure r
        [] -> fail "no search results"
      first.name `shouldBe` "ok computer"
      first.container `shouldBe` YesNo False
      first.playable `shouldBe` YesNo True
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
      r.container `shouldBe` YesNo True
      r.cid `shouldBe` Just (ContainerId "77")
      r.mid `shouldBe` Nothing
      r.album `shouldBe` Nothing
      r.album_id `shouldBe` Nothing

  -- The criteria are per source: the device says which scid means what.
  describe "criterionFor" $ do
    it "finds the scid of each kind in the Tidal criteria reply" $ do
      crits <- payloadOf "search_criteria.json" >>= decodeAs @[SearchCriterion]
      map (`criterionFor` crits) [Artist, Album, Track, Playlist]
        `shouldBe` map (Just . SearchCriteriaId) [1, 2, 3, 6]

    it "finds nothing when the source offers no criteria" $
      criterionFor Track [] `shouldBe` Nothing

  -- From what the device sent to what the scenario can play.
  describe "toFound" $ do
    it "turns searched songs into tracks of the SEARCHED_TRACKS container" $ do
      results <- payloadOf "search_ok_computer.json" >>= decodeAs @[SearchResult]
      let found = map (toFound (SourceId 10) "Ok Computer") results
      length found `shouldBe` 50
      map (.kind) found `shouldSatisfy` all (== "song")
      let targets = [t | Found{target = Just t} <- found]
      length targets `shouldBe` 50
      targets `shouldSatisfy` all (\case PlayTrack (SourceId 10) (ContainerId "SEARCHED_TRACKS-Ok Computer") _ -> True; _ -> False)
      case found of
        f : _ -> do
          f.name `shouldBe` "ok computer"
          f.artist `shouldBe` Just "Sef"
          f.target `shouldBe` Just (PlayTrack (SourceId 10) (ContainerId "SEARCHED_TRACKS-Ok Computer") (MediaId "443242295"))
        [] -> expectationFailure "no results"

    it "turns a container into PlayContainer with its cid" $ do
      r <- either fail pure (eitherDecodeStrict @SearchResult "{\"container\": \"yes\", \"cid\": \"77\", \"type\": \"album\", \"playable\": \"yes\", \"name\": \"OK Computer\", \"image_url\": \"x\"}")
      (toFound (SourceId 10) "q" r).target `shouldBe` Just (PlayContainer (SourceId 10) (ContainerId "77"))

    it "gives no target to an item the device marks unplayable" $ do
      r <- either fail pure (eitherDecodeStrict @SearchResult "{\"container\": \"no\", \"mid\": \"5\", \"type\": \"song\", \"playable\": \"no\", \"name\": \"x\", \"image_url\": \"x\"}")
      (toFound (SourceId 10) "q" r).target `shouldBe` Nothing

    it "gives no target to a song without a mid" $ do
      r <- either fail pure (eitherDecodeStrict @SearchResult "{\"container\": \"no\", \"type\": \"song\", \"playable\": \"yes\", \"name\": \"x\", \"image_url\": \"x\"}")
      (toFound (SourceId 10) "q" r).target `shouldBe` Nothing

  -- Two spellings of yes in one protocol: search results say "yes"/"no",
  -- sources say "true"/"false". Each parser accepts only its own.
  describe "YesNo and TrueFalse" $ do
    it "YesNo rejects true/false" $
      parseEither (parseJSON @YesNo) (String "true") `shouldSatisfy` isLeft
    it "TrueFalse rejects yes/no" $
      parseEither (parseJSON @TrueFalse) (String "yes") `shouldSatisfy` isLeft
