-- | The browse side of the protocol (spec 4.4): music sources, what can be
--   searched in them, what a search returns, and what of that can be played
--   or queued.
module Heos.Types.Browse where

import Data.Aeson (FromJSON (parseJSON), genericParseJSON)
import Data.List (find)
import Data.Text (Text)
import GHC.Generics (Generic)
import Heos.Json (TrueFalse, YesNo (..), heosJson)
import Heos.ToParam (ToParam (..))

newtype SourceId = SourceId Int
  deriving (Show, Eq, Ord)
  deriving newtype (FromJSON)
instance ToParam SourceId where
  toParam (SourceId sid) = toParam sid

newtype ContainerId = ContainerId Text
  deriving (Show, Eq)
  deriving newtype (FromJSON)
instance ToParam ContainerId where
  toParam (ContainerId cid) = cid

newtype MediaId = MediaId Text
  deriving (Show, Eq)
  deriving newtype (FromJSON)
instance ToParam MediaId where
  toParam (MediaId mid) = mid

newtype SearchCriteriaId = SearchCriteriaId Int
  deriving (Show, Eq)
  deriving newtype (FromJSON)
instance ToParam SearchCriteriaId where
  toParam (SearchCriteriaId scid) = toParam scid

data SearchKind = ArtistKind | AlbumKind | TrackKind | PlaylistKind deriving (Show, Eq)

-- aid in 4.4.11 / 4.4.12
data QueueMode = AddPlayNow | AddPlayNext | AddToEnd | AddReplaceAndPlay deriving (Show, Eq)
instance ToParam QueueMode where
  toParam = \case
    AddPlayNow -> "1"
    AddPlayNext -> "2"
    AddToEnd -> "3"
    AddReplaceAndPlay -> "4"

data Playable
  = PlayTrack SourceId ContainerId MediaId
  | PlayContainer SourceId ContainerId
  deriving (Show, Eq)

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
