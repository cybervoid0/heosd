module Heos.Envelope (Envelope (..), HeosHeader (..), HeosResult (..), IncomingEnvelope (..), classifyEnvelope, HeosOutcome (..), HeosError (..)) where

import Data.Aeson ((.:), (.:?))
import Data.Aeson.Types (FromJSON (..), Options (..), Value, defaultOptions, genericParseJSON, withObject)
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

data HeosOutcome = Succeeded | Failed HeosError deriving (Show, Eq, Generic)

data HeosHeader = HeosHeader {command :: Text, outcome :: Maybe HeosOutcome, message :: Maybe Text} deriving (Show, Eq, Generic)
instance FromJSON HeosHeader where
  parseJSON = withObject "HeosHeader" $ \o -> do
    command <- o .: "command"
    message <- o .:? "message"
    result :: Maybe HeosResult <- o .:? "result"
    let outcome = do
          r <- result
          case r of
            Success -> pure Succeeded
            Fail -> pure . Failed . findError . fromMaybe "" $ message
    pure (HeosHeader{command, outcome, message})

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
