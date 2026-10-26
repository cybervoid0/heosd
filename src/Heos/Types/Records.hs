module Heos.Types.Records where

import Data.Aeson (FromJSON (parseJSON), genericParseJSON)
import Data.List (find)
import Data.Text (Text)
import GHC.Generics (Generic)
import Heos.Json (heosJson)
import Heos.Types.Enums (Network, Playable (..), SearchKind (..))
import Heos.Types.Newtypes (ContainerId (..), MediaId, PlayerId, QueueId, SearchCriteriaId, SourceId, TrueFalse, YesNo (..))

data Found = Found
  { name :: Text
  , artist :: Maybe Text
  , album :: Maybe Text
  , kind :: Text
  , target :: Maybe Playable -- Nothing, если playable = no
  }
  deriving (Show, Eq)

toFound :: SourceId -> Text -> SearchResult -> Found
toFound sid query r@(SearchResult{name, artist, album, _type}) = Found{name, artist, album, kind = _type, target}
 where
  target = case (r.playable, r.container, r.cid, r.mid) of
    (YesNo False, _, _, _) -> Nothing -- нельзя играть
    (_, YesNo True, Just cid, _) -> Just (PlayContainer sid cid) -- альбом, плейлист
    (_, YesNo False, _, Just mid) -> Just (PlayTrack sid searchedTracks mid)
    _ -> Nothing -- не хватает идентификатора
  searchedTracks = ContainerId ("SEARCHED_TRACKS-" <> query)

data MusicSource = MusicSource
  { name :: Text
  , image_url :: Text
  , _type :: Text
  , sid :: SourceId
  , available :: TrueFalse
  , service_username :: Maybe Text
  }
  deriving (Eq, Show, Generic)
instance FromJSON MusicSource where
  parseJSON = genericParseJSON heosJson

data Player = Player
  { name :: Text
  , pid :: PlayerId
  , model :: Text
  , version :: Text
  , ip :: Text
  , network :: Network
  , lineout :: Int
  , serial :: Text
  }
  deriving (Eq, Show, Generic)
instance FromJSON Player where
  parseJSON = genericParseJSON heosJson

data SearchCriterion = SearchCriterion
  { name :: Text
  , scid :: SearchCriteriaId
  , wildcard :: YesNo
  , cid :: Maybe ContainerId
  , playable :: Maybe YesNo
  }
  deriving (Eq, Show, Generic)
instance FromJSON SearchCriterion where
  parseJSON = genericParseJSON heosJson

criterionFor :: SearchKind -> [SearchCriterion] -> Maybe SearchCriteriaId
criterionFor kind = fmap (.scid) . find (\sc -> sc.name == wanted)
 where
  wanted = case kind of
    ArtistKind -> "Artist"
    AlbumKind -> "Album"
    TrackKind -> "Track"
    PlaylistKind -> "Playlist"

data SearchResult = SearchResult
  { container :: YesNo
  , mid :: Maybe MediaId
  , cid :: Maybe ContainerId
  , _type :: Text
  , artist :: Maybe Text
  , album :: Maybe Text
  , album_id :: Maybe Text
  , playable :: YesNo
  , name :: Text
  , image_url :: Text
  }
  deriving (Eq, Show, Generic)
instance FromJSON SearchResult where
  parseJSON = genericParseJSON heosJson

data Track = Track
  { _type :: Text
  , song :: Text
  , station :: Maybe Text
  , album :: Text
  , artist :: Text
  , image_url :: Text
  , album_id :: Text
  , mid :: MediaId
  , qid :: QueueId
  , sid :: SourceId
  }
  deriving (Show, Eq, Generic)
instance FromJSON Track where
  parseJSON = genericParseJSON heosJson