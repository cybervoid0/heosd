module Heos.Envelope (Envelope (..), HeosHeader (..), HeosResult (..)) where

import Data.Aeson.Types (FromJSON (..), Options (..), Value, defaultOptions, genericParseJSON)
import Data.Text (Text)
import GHC.Generics

import Data.Char (toLower)

-- import Data.Text qualified as T
lowerTags :: Options
lowerTags = defaultOptions{constructorTagModifier = map toLower}

data HeosResult = Success | Fail deriving (Show, Eq, Generic)
instance FromJSON HeosResult where
  -- parse "success" as Success and "fail" to Fail
  parseJSON = genericParseJSON lowerTags

data HeosHeader = HeosHeader {command :: Text, result :: HeosResult, message :: Text} deriving (Show, Eq, Generic)
instance FromJSON HeosHeader

data Envelope = Envelope
  { heos :: HeosHeader
  , payload :: Maybe Value
  , options :: Maybe [Value]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Envelope
