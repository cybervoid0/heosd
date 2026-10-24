module Heos.Player (
  Player (..),
  GroupId (..),
  NowPlaying (..),
  PlayerId (..),
  QueueId (..),
  Network (..),
  PlayState (..),
  parsePlayState,
  OnOff (..),
) where

import Data.Aeson (FromJSON (parseJSON), genericParseJSON, withText)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Heos.Json (heosJson)
import Heos.Media (MediaId)
import Heos.ToParam (ToParam (..))

--- PlayerId

newtype PlayerId = PlayerId Int
  deriving (Show, Eq)
  deriving newtype (FromJSON)
instance ToParam PlayerId where
  toParam (PlayerId p) = toParam p

newtype GroupId = GroupId Int deriving (Show, Eq)
instance ToParam GroupId where
  toParam (GroupId g) = toParam g

--- Network

data Network = WiFi | Wired deriving (Show, Eq)
instance FromJSON Network where
  parseJSON = withText "network" $ \case
    "wifi" -> pure WiFi
    "wired" -> pure Wired
    other -> fail ("unknown network: " <> T.unpack other)

--- QID
newtype QueueId = QueueId Int
  deriving (Show, Eq)
  deriving newtype (FromJSON)
instance ToParam QueueId where
  toParam (QueueId qid) = toParam qid

-- Collections

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

data NowPlaying = NowPlaying
  { _type :: Text
  , song :: Text
  , station :: Maybe Text
  , album :: Text
  , artist :: Text
  , image_url :: Text
  , album_id :: Text
  , mid :: MediaId
  , qid :: QueueId
  }
  deriving (Eq, Show, Generic)
instance FromJSON NowPlaying where
  parseJSON = genericParseJSON heosJson

data PlayState = Play | Pause | Stop deriving (Show, Eq, Enum, Bounded)
instance ToParam PlayState where
  toParam = \case
    Play -> "play"
    Pause -> "pause"
    Stop -> "stop"
parsePlayState :: Text -> Maybe PlayState
parsePlayState t = find ((== t) . toParam) [minBound .. maxBound]

data OnOff = On | Off deriving (Show, Eq)
instance ToParam OnOff where
  toParam status = case status of
    On -> "on"
    Off -> "off"