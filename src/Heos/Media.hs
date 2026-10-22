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
) where

import Data.Aeson (FromJSON (parseJSON), genericParseJSON, withText)
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

-- SCID
newtype SearchCriteriaId = SearchCriteriaId Int
  deriving (Show, Eq)
  deriving newtype (FromJSON)
instance ToParam SearchCriteriaId where
  toParam (SearchCriteriaId scid) = toParam scid

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
