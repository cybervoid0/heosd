module Heos.Envelope where

import Data.Aeson.Types (FromJSON, Value)
import Data.Text (Text)

-- import Data.Text qualified as T
import GHC.Generics (Generic)

data Result = Success | Failure deriving stock (Show, Eq, Generic)
instance FromJSON Result

data Heos = Heos {command :: Text, result :: Result, message :: Text} deriving stock (Show, Eq, Generic)
instance FromJSON Heos

data Envelope = Envelope
  { heos :: Heos
  , payload :: Maybe Value
  , options :: Maybe [Value]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Envelope
