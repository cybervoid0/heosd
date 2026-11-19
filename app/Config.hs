module Config (
  Config (..),
  ServerConfig (..),
  DeviceConfig (..),
  RetryConfig (..),
  LogLevel (..),
) where

import Data.Aeson (withText)
import Data.Text (Text)
import Data.Yaml (FromJSON (..))
import GHC.Generics (Generic)
import Heos.Types.System (Credentials)

-- | What the daemon reads from its YAML file. Every section is optional:
--   an empty file is a valid config, and the defaults live in code
--   ('Heos.Device.Parameters'), not here.
data Config = Config
  { server :: Maybe ServerConfig
  , device :: Maybe DeviceConfig
  , account :: Maybe Credentials
  -- ^ the HEOS account for sign_in; without it streaming services are off
  , logLevel :: Maybe LogLevel
  }
  deriving (Show, Eq, Generic)

instance FromJSON Config

-- | Where the MCP server listens.
data ServerConfig = ServerConfig
  { host :: Maybe Text
  , port :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON ServerConfig

-- | What differs about the device from one home to the next. Protocol
--   constants (the HEOS port, the line limit) are not here on purpose.
data DeviceConfig = DeviceConfig
  { address :: Maybe Text
  -- ^ skip discovery and connect here
  , commandTimeout :: Maybe Double
  -- ^ seconds
  , heartbeatDelay :: Maybe Double
  -- ^ seconds
  , retry :: Maybe RetryConfig
  , avr :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON DeviceConfig

-- | Exponential backoff between connection attempts, in seconds.
data RetryConfig = RetryConfig
  { base :: Double
  , cap :: Double
  }
  deriving (Show, Eq, Generic)

instance FromJSON RetryConfig

data LogLevel = Debug | Info | Warn | Error
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Lower-case in the file: @level: info@.
instance FromJSON LogLevel where
  parseJSON = withText "LogLevel" $ \case
    "debug" -> pure Debug
    "info" -> pure Info
    "warn" -> pure Warn
    "error" -> pure Error
    other -> fail ("unknown log level: " <> show other <> ", expected debug, info, warn or error")
