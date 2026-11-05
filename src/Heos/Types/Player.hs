-- | The player side of the protocol (spec 4.2, groups 4.3): the player
--   itself, how it plays, and what it plays.
module Heos.Types.Player where

import Data.Aeson (FromJSON (parseJSON), genericParseJSON, withText)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Heos.Json (heosJson)
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
instance FromJSON Player where
  parseJSON = genericParseJSON heosJson

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
