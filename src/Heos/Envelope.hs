module Heos.Envelope (Envelope (..), HeosHeader (..), HeosResult (..), IncomingEnvelope (..), classifyEnvelope, HeosEvent (..), HeosCommand (..), CommandGroup (..), parseCommand, parseGroup, EnvelopeError (..)) where

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

data EnvelopeError = CommandFmtFailure Text | UnknownGroup Text deriving (Show, Eq)

data CommandGroup = Event | System | Player | Browse | Group deriving (Show, Eq)
parseGroup :: Text -> Either EnvelopeError CommandGroup
parseGroup txt = case txt of
  "event" -> Right Event
  "system" -> Right System
  "player" -> Right Player
  "browse" -> Right Browse
  "group" -> Right Group
  g -> Left $ UnknownGroup g

data HeosCommand = HeosCommand {group :: CommandGroup, name :: Text} deriving (Show, Eq)
parseCommand :: Text -> Either EnvelopeError HeosCommand
parseCommand txt = case T.splitOn "/" txt of
  [g, name]
    | not (T.null g)
    , not (T.null name) -> do
        group <- parseGroup g
        pure $ HeosCommand group name
  _ -> Left $ CommandFmtFailure txt

newtype HeosEvent = HeosEvent {name :: Text} deriving (Show, Eq)

data IncomingEnvelope
  = IncomingEvent HeosEvent Envelope
  | IncomingPending Envelope
  | IncomingResponse Envelope
  deriving (Show, Eq)

classifyEnvelope :: Envelope -> Either EnvelopeError IncomingEnvelope
classifyEnvelope env = do
  HeosCommand cmd name <- parseCommand env.heos.command
  case cmd of
    Event -> pure $ IncomingEvent (HeosEvent name) env
    _
      | isPending -> pure $ IncomingPending env
      | otherwise -> pure $ IncomingResponse env
 where
  isPending = "command under process" `T.isInfixOf` msg
  msg = fromMaybe "" env.heos.message
