module Heos.Types.Newtypes where

import Data.Aeson (FromJSON (parseJSON), withText)
import Data.Text (Text)
import Data.Text qualified as T
import Heos.ToParam (ToParam (..))

newtype ContainerId = ContainerId Text
  deriving (Show, Eq)
  deriving newtype (FromJSON)
instance ToParam ContainerId where
  toParam (ContainerId cid) = cid

newtype GroupId = GroupId Int deriving (Show, Eq)
instance ToParam GroupId where
  toParam (GroupId g) = toParam g

newtype MediaId = MediaId Text
  deriving (Show, Eq)
  deriving newtype (FromJSON)
instance ToParam MediaId where
  toParam (MediaId mid) = mid

newtype PlayerId = PlayerId Int
  deriving (Show, Eq)
  deriving newtype (FromJSON)
instance ToParam PlayerId where
  toParam (PlayerId p) = toParam p

newtype QueueId = QueueId Int
  deriving (Show, Eq)
  deriving newtype (FromJSON)
instance ToParam QueueId where
  toParam (QueueId qid) = toParam qid

newtype SearchCriteriaId = SearchCriteriaId Int
  deriving (Show, Eq)
  deriving newtype (FromJSON)
instance ToParam SearchCriteriaId where
  toParam (SearchCriteriaId scid) = toParam scid

newtype SourceId = SourceId Int
  deriving (Show, Eq)
  deriving newtype (FromJSON)
instance ToParam SourceId where
  toParam (SourceId sid) = toParam sid

newtype TrueFalse = TrueFalse Bool deriving (Show, Eq)
instance FromJSON TrueFalse where
  parseJSON = withText "true/false" $ \case
    "true" -> pure (TrueFalse True)
    "false" -> pure (TrueFalse False)
    other -> fail ("expected true or false, got " <> T.unpack other)

newtype YesNo = YesNo Bool deriving (Show, Eq)
instance FromJSON YesNo where
  parseJSON = withText "yes/no" $ \case
    "yes" -> pure (YesNo True)
    "no" -> pure (YesNo False)
    other -> fail ("expected yes or no, got " <> T.unpack other)