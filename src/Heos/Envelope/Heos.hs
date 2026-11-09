module Heos.Envelope.Heos (
  -- * Raw envelope, as decoded from JSON
  Envelope (..),
  HeosHeader (..),
  HeosResult (..),

  -- * Classified envelope
  IncomingEnvelope (..),
  HeosOutcome (..),
  HeosError (..),
  findError,
  HeosEvent (..),
  classifyEnvelope,

  -- * Reading a reply
  ProtocolError (..),
  decodePayload,
  fromMessage,
) where

import Control.Exception (Exception, throwIO)
import Data.Aeson.Types (FromJSON (..), Options (..), Value, defaultOptions, genericParseJSON, parseEither)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Heos.Url (UrlParam (UrlFlag), decodeQuery, lookupParam)
import Text.Read (readMaybe)

data HeosError = HeosError
  { eid :: Maybe Int
  , text :: Maybe Text
  }
  deriving (Show, Eq)
instance Exception HeosError
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

data HeosHeader = HeosHeader
  { command :: Text
  , result :: Maybe HeosResult
  , message :: Maybe Text
  }
  deriving (Show, Eq, Generic)
instance FromJSON HeosHeader

data Envelope = Envelope
  { heos :: HeosHeader
  , payload :: Maybe Value
  , options :: Maybe [Value]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Envelope

data HeosOutcome = Succeeded | Failed HeosError deriving (Show, Eq)

data HeosEvent = HeosEvent
  { name :: Text
  , params :: [UrlParam]
  }
  deriving (Show, Eq)

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

-- | A reply that cannot be read as what the caller expected: no payload
--   where one is due, a payload of the wrong shape, a message without the
--   key. The connection is fine; the device said something unforeseen.
newtype ProtocolError = ProtocolError Text deriving (Show)

instance Exception ProtocolError

-- the payload of a reply, decoded into whatever the operation returns
decodePayload :: (FromJSON a) => Envelope -> IO a
decodePayload env = case env.payload of
  Nothing -> throwIO (ProtocolError "reply without payload")
  Just v -> either (throwIO . ProtocolError . T.pack) pure (parseEither parseJSON v)

-- a value from the reply's message: "pid=…&key=15" -> 15
fromMessage :: Text -> (Text -> Maybe a) -> Envelope -> IO a
fromMessage key parse env =
  maybe (throwIO (ProtocolError ("no " <> key <> " in " <> T.pack (show env.heos.message)))) pure $
    env.heos.message >>= lookupParam key . decodeQuery >>= parse
