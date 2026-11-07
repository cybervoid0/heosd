-- | The player side of the protocol (spec 4.2, groups 4.3): the player
--   itself, how it plays, and what it plays.
module Heos.Types.Player where

import Data.Aeson (FromJSON (parseJSON), withObject, withText, (.:), (.:?))
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Heos.ToParam (ToParam (..))
import Heos.Types.Browse (MediaId, SourceId)

newtype PlayerId = PlayerId Int
  deriving (Show, Eq)
  deriving newtype (FromJSON)
instance ToParam PlayerId where
  toParam (PlayerId p) = toParam p

newtype GroupId = GroupId Int deriving (Show, Eq)
instance ToParam GroupId where
  toParam (GroupId g) = toParam g

data Network = WiFi | Wired deriving (Show, Eq)
instance FromJSON Network where
  parseJSON = withText "network" $ \case
    "wifi" -> pure WiFi
    "wired" -> pure Wired
    other -> fail ("unknown network: " <> T.unpack other)

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
instance FromJSON Player

data PlayState = Play | Pause | Stop deriving (Show, Eq, Enum, Bounded)
instance ToParam PlayState where
  toParam = \case
    Play -> "play"
    Pause -> "pause"
    Stop -> "stop"
parsePlayState :: Text -> Maybe PlayState
parsePlayState t = find ((== t) . toParam) [minBound .. maxBound]

data RepeatMode = RepeatAll | RepeatOne | RepeatOff deriving (Show, Eq)
instance ToParam RepeatMode where
  toParam = \case
    RepeatAll -> "on_all"
    RepeatOne -> "on_one"
    RepeatOff -> "off"

newtype QueueId = QueueId Int
  deriving (Show, Eq)
  deriving newtype (FromJSON)
instance ToParam QueueId where
  toParam (QueueId qid) = toParam qid

data Track = Track
  { trackType :: Text
  , song :: Text
  , station :: Maybe Text
  , album :: Text
  , artist :: Text
  , imageUrl :: Text
  , albumId :: Text
  , mid :: MediaId
  , qid :: QueueId
  , sid :: SourceId
  }
  deriving (Show, Eq)
instance FromJSON Track where
  parseJSON = withObject "Track" $ \obj -> do
    trackType <- obj .: "type"
    song <- obj .: "song"
    station <- obj .:? "station"
    album <- obj .: "album"
    artist <- obj .: "artist"
    imageUrl <- obj .: "image_url"
    albumId <- obj .: "album_id"
    mid <- obj .: "mid"
    qid <- obj .: "qid"
    sid <- obj .: "sid"
    pure $ Track{trackType, song, station, album, artist, imageUrl, albumId, mid, sid, qid}
data QueueItem = QueueItem
  { song :: Text
  , album :: Text
  , artist :: Text
  , imageUrl :: Text
  , qid :: QueueId
  , mid :: MediaId
  , albumId :: Text
  }
  deriving (Show, Eq)
instance FromJSON QueueItem where
  parseJSON = withObject "QueueItem" $ \obj -> do
    song <- obj .: "song"
    album <- obj .: "album"
    artist <- obj .: "artist"
    imageUrl <- obj .: "image_url"
    qid <- obj .: "qid"
    mid <- obj .: "mid"
    albumId <- obj .: "album_id"
    pure $ QueueItem{song, album, artist, imageUrl, qid, mid, albumId}
