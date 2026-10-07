module Heos.Envelope.Internal where

import Data.Aeson.Types (FromJSON (..), Options (..), Value, defaultOptions, genericParseJSON)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Heos.Url (UrlParam (UrlFlag), decodeQuery, lookupParam)
import Text.Read (readMaybe)

data HeosError = HeosError {eid :: Maybe Int, text :: Maybe Text} deriving (Show, Eq)
findError :: Text -> HeosError
findError msg = HeosError{eid, text}
 where
  params = decodeQuery msg
  eidTxt = fromMaybe "" (lookupParam "eid" params)
  eid = readMaybe (T.unpack eidTxt) :: Maybe Int
  text = lookupParam "text" params

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

data HeosOutcome = Succeeded | Failed HeosError deriving (Show, Eq)

data HeosEvent = HeosEvent {name :: Text, params :: [UrlParam]} deriving (Show, Eq)

data IncomingEnvelope
  = IncomingEvent HeosEvent Envelope
  | IncomingPending Envelope
  | IncomingResponse HeosOutcome Envelope
  deriving (Show, Eq)

classifyEnvelope :: Envelope -> IncomingEnvelope
classifyEnvelope env
  -- stripPrefix полезная функция, надо запомнить: удаляет префикс из текста и
  -- возвращает то что осталось
  | Just name <- T.stripPrefix "event/" env.heos.command = IncomingEvent (HeosEvent name (maybe [] decodeQuery env.heos.message)) env
  | isPending = IncomingPending env
  | otherwise = IncomingResponse outcome env
 where
  isPending = UrlFlag "command under process" `elem` decodeQuery msg
  msg = fromMaybe "" env.heos.message
  outcome = case env.heos.result of
    Just Fail -> Failed $ findError msg
    _ -> Succeeded
