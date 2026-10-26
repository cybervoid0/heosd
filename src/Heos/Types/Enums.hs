module Heos.Types.Enums where

import Data.Aeson (FromJSON (..), withText)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Heos.ToParam (ToParam (..))
import Heos.Types.Newtypes (ContainerId, MediaId, SourceId)

data Credentials = Credentials {login :: Text, password :: Text} deriving (Eq)
instance Show Credentials where
  show (Credentials login _) = "Login=" <> T.unpack login <> " Password=<hidden>"

data Network = WiFi | Wired deriving (Show, Eq)
instance FromJSON Network where
  parseJSON = withText "network" $ \case
    "wifi" -> pure WiFi
    "wired" -> pure Wired
    other -> fail ("unknown network: " <> T.unpack other)

data OnOff = On | Off deriving (Show, Eq)
instance ToParam OnOff where
  toParam status = case status of
    On -> "on"
    Off -> "off"

data Playable
  = PlayTrack SourceId ContainerId MediaId
  | PlayContainer SourceId ContainerId
  deriving (Show, Eq)

data PlayState = Play | Pause | Stop deriving (Show, Eq, Enum, Bounded)
instance ToParam PlayState where
  toParam = \case
    Play -> "play"
    Pause -> "pause"
    Stop -> "stop"
parsePlayState :: Text -> Maybe PlayState
parsePlayState t = find ((== t) . toParam) [minBound .. maxBound]

-- aid in 4.4.11 / 4.4.12
data QueueMode = AddPlayNow | AddPlayNext | AddToEnd | AddReplaceAndPlay deriving (Show, Eq)
instance ToParam QueueMode where
  toParam = \case
    AddPlayNow -> "1"
    AddPlayNext -> "2"
    AddToEnd -> "3"
    AddReplaceAndPlay -> "4"

data RepeatMode = RepeatAll | RepeatOne | RepeatOff deriving (Show, Eq)
instance ToParam RepeatMode where
  toParam = \case
    RepeatAll -> "on_all"
    RepeatOne -> "on_one"
    RepeatOff -> "off"

data SearchKind = ArtistKind | AlbumKind | TrackKind | PlaylistKind deriving (Show, Eq)