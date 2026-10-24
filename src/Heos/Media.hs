module Heos.Media (
  MediaId (..),
  SourceId (..),
  ContainerId (..),
  SearchCriteriaId (..),
  SearchResult (..),
  YesNo (..),
  TrueFalse (..),
  SearchKind (..),
  MusicSource (..),
  Playable (..),
  SearchCriterion (..),
  Found (..),
  toFound,
  criterionFor,
) where

import Data.Aeson (FromJSON (parseJSON), genericParseJSON, withText)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Heos.Json (heosJson)
import Heos.ToParam (ToParam (..))

-- | MID
newtype MediaId = MediaId Text
  deriving (Show, Eq)
  deriving newtype (FromJSON)

instance ToParam MediaId where
  toParam (MediaId mid) = mid

-- | SID
newtype SourceId = SourceId Int
  deriving (Show, Eq)
  deriving newtype (FromJSON)

instance ToParam SourceId where
  toParam (SourceId sid) = toParam sid

-- CID
newtype ContainerId = ContainerId Text
  deriving (Show, Eq)
  deriving newtype (FromJSON)
instance ToParam ContainerId where
  toParam (ContainerId cid) = cid

newtype YesNo = YesNo Bool deriving (Show, Eq)
instance FromJSON YesNo where
  parseJSON = withText "yes/no" $ \case
    "yes" -> pure (YesNo True)
    "no" -> pure (YesNo False)
    other -> fail ("expected yes or no, got " <> T.unpack other)

newtype TrueFalse = TrueFalse Bool deriving (Show, Eq)
instance FromJSON TrueFalse where
  parseJSON = withText "true/false" $ \case
    "true" -> pure (TrueFalse True)
    "false" -> pure (TrueFalse False)
    other -> fail ("expected true or false, got " <> T.unpack other)

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

data SearchKind = Artist | Album | Track | Playlist deriving (Show, Eq)

data Playable
  = PlayTrack SourceId ContainerId MediaId
  | PlayContainer SourceId ContainerId
  deriving (Show, Eq)

-- SCID
newtype SearchCriteriaId = SearchCriteriaId Int
  deriving (Show, Eq)
  deriving newtype (FromJSON)
instance ToParam SearchCriteriaId where
  toParam (SearchCriteriaId scid) = toParam scid

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

-- the search criterion of this kind, if the source offers one
criterionFor :: SearchKind -> [SearchCriterion] -> Maybe SearchCriteriaId
criterionFor kind = fmap (.scid) . find (\sc -> sc.name == wanted)
 where
  wanted = case kind of
    Artist -> "Artist"
    Album -> "Album"
    Track -> "Track"
    Playlist -> "Playlist"

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