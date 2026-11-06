-- | The browse side of the protocol (spec 4.4): music sources, what can be
--   searched in them, what a search returns, and what of that can be played
--   or queued.
module Heos.Types.Browse where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import Data.List (find)
import Data.Text (Text)
import Heos.Json (trueFalse, yesNo)
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
  , imageUrl :: Text
  , sourceType :: Text
  , sid :: SourceId
  , available :: Bool
  , serviceUsername :: Maybe Text
  }
  deriving (Eq, Show)
instance FromJSON MusicSource where
  parseJSON = withObject "MusicSource" $ \obj -> do
    name <- obj .: "name"
    imageUrl <- obj .: "image_url"
    sourceType <- obj .: "type"
    sid <- obj .: "sid"
    available <- obj .: "available" >>= trueFalse
    serviceUsername <- obj .:? "service_username"
    pure $ MusicSource{name, imageUrl, sourceType, sid, available, serviceUsername}

data SearchCriterion = SearchCriterion
  { name :: Text
  , scid :: SearchCriteriaId
  , wildcard :: Bool
  , cid :: Maybe ContainerId
  , playable :: Maybe Bool
  }
  deriving (Eq, Show)
instance FromJSON SearchCriterion where
  parseJSON = withObject "SearchCriterion" $ \obj -> do
    name <- obj .: "name"
    scid <- obj .: "scid"
    wildcard <- obj .: "wildcard" >>= yesNo
    cid <- obj .:? "cid"
    playable <- obj .:? "playable" >>= traverse yesNo
    pure $ SearchCriterion{name, scid, wildcard, cid, playable}

criterionFor :: SearchKind -> [SearchCriterion] -> Maybe SearchCriteriaId
criterionFor kind = ((.scid) <$>) . find (\sc -> sc.name == wanted)
 where
  wanted = case kind of
    ArtistKind -> "Artist"
    AlbumKind -> "Album"
    TrackKind -> "Track"
    PlaylistKind -> "Playlist"

data SearchResult = SearchResult
  { container :: Bool
  , mid :: Maybe MediaId
  , cid :: Maybe ContainerId
  , resultType :: Text
  , artist :: Maybe Text
  , album :: Maybe Text
  , albumId :: Maybe Text
  , playable :: Bool
  , name :: Text
  , imageUrl :: Text
  }
  deriving (Eq, Show)
instance FromJSON SearchResult where
  parseJSON = withObject "SearchResult" $ \obj -> do
    container <- obj .: "container" >>= yesNo
    mid <- obj .:? "mid"
    cid <- obj .:? "cid"
    resultType <- obj .: "type"
    artist <- obj .:? "artist"
    album <- obj .:? "album"
    albumId <- obj .:? "album_id"
    playable <- obj .: "playable" >>= yesNo
    name <- obj .: "name"
    imageUrl <- obj .: "image_url"
    pure $ SearchResult{container, mid, cid, resultType, artist, album, albumId, playable, name, imageUrl}

data Found = Found
  { name :: Text
  , artist :: Maybe Text
  , album :: Maybe Text
  , kind :: Text
  , target :: Playable -- Nothing, если playable = no
  }
  deriving (Show, Eq)

toFound :: SourceId -> SearchResult -> Maybe Found
toFound sid r@(SearchResult{name, artist, album, resultType}) =
  case (r.playable, r.container, r.cid, r.mid, r.albumId) of
    (False, _, _, _, _) -> Nothing -- нельзя играть
    (_, True, Just cid, _, _) -> pure $ found (PlayContainer sid cid) -- альбом, плейлист
    (_, False, _, Just mid, Just aid) ->
      -- TODO: префикс LIBALBUM работает только с Tidal. Установить префиксы для других сервисов
      let cid = ContainerId ("LIBALBUM-" <> aid)
       in pure $ found (PlayTrack sid cid mid)
    _ -> Nothing -- не хватает идентификатора
 where
  found target = Found{name, artist, album, kind = resultType, target}
