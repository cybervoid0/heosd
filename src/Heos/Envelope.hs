module Heos.Envelope (Envelope (..), HeosHeader (..), HeosResult (..), IncomingEnvelope (..), classifyEnvelope) where

import Data.Aeson.Types (FromJSON (..), Options (..), Value, defaultOptions, genericParseJSON)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Heos.Url (UrlParam (UrlFlag), decodeQuery)

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
  = IncomingEvent Text Envelope
  | IncomingPending Envelope
  | IncomingResponse Envelope
  deriving (Show, Eq)

classifyEnvelope :: Envelope -> IncomingEnvelope
classifyEnvelope env
  -- stripPrefix полезная функция, надо запомнить: удаляет префикс из текста и
  -- возвращает то что осталось
  | Just name <- T.stripPrefix "event/" env.heos.command = IncomingEvent name env
  | isPending = IncomingPending env
  | otherwise = IncomingResponse env
 where
  isPending = UrlFlag "command under process" `elem` decodeQuery msg
  msg = fromMaybe "" env.heos.message
