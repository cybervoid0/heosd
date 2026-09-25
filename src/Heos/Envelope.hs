module Heos.Envelope (Envelope (..), HeosHeader (..), HeosResult (..), IncomingEnvelope (..), classifyEnvelope) where

import Data.Aeson.Types (FromJSON (..), Options (..), Value, defaultOptions, genericParseJSON)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics

lowerTags :: Options
lowerTags = defaultOptions{constructorTagModifier = map toLower}

data HeosResult = Success | Fail deriving (Show, Eq, Generic)
instance FromJSON HeosResult where
  -- parse "success" as Success and "fail" to Fail
  parseJSON = genericParseJSON lowerTags

data HeosHeader = HeosHeader {command :: Text, result :: Maybe HeosResult, message :: Maybe Text} deriving (Show, Eq, Generic)
instance FromJSON HeosHeader

data Envelope = Envelope
  { heos :: HeosHeader
  , payload :: Maybe Value
  , options :: Maybe [Value]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Envelope

data IncomingEnvelope
  = IncomingEvent Envelope
  | IncomingPending Envelope
  | IncomingResponse Envelope

classifyEnvelope :: Envelope -> IncomingEnvelope
classifyEnvelope env
  | "event/" `T.isPrefixOf` env.heos.command = IncomingEvent env
  | isPending = IncomingPending env
  | otherwise = IncomingResponse env
 where
  isPending = "command under process" `T.isInfixOf` msg
  msg = fromMaybe "" env.heos.message