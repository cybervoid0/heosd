module Heos.Media (
  MediaId (..),
  SourceId (..),
  ContainerId (..),
  SearchCriteriaId (..),
) where

import Data.Text (Text)
import Heos.ToParam (ToParam (..))

newtype MediaId = MediaId Text deriving (Show, Eq)
instance ToParam MediaId where
  toParam (MediaId mid) = mid

newtype SourceId = SourceId Int deriving (Show, Eq)
instance ToParam SourceId where
  toParam (SourceId sid) = toParam sid

newtype ContainerId = ContainerId Text deriving (Show, Eq)
instance ToParam ContainerId where
  toParam (ContainerId cid) = cid

newtype SearchCriteriaId = SearchCriteriaId Int deriving (Show, Eq)
instance ToParam SearchCriteriaId where
  toParam (SearchCriteriaId scid) = toParam scid
