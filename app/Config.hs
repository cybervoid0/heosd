module Config (
  Config (..),
  ServerConfig (..),
  DeviceConfig (..),
  RetryConfig (..),
  Address (..),
) where

import Data.Aeson (withText)
import Data.IP (IPv4)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml (FromJSON (..))
import GHC.Generics (Generic)
import Heos.Logger (Severity)
import Heos.Types.System (Credentials)
import Text.Read (readMaybe)

newtype Address = Address {ipv4 :: IPv4} deriving (Show, Eq)

instance FromJSON Address where
  parseJSON = withText "Address" $ \t ->
    maybe (fail ("not an IPv4 address: " <> show t)) (pure . Address) (readMaybe (T.unpack t))

-- | What the daemon reads from its YAML file. Every section is optional:
--   an empty file is a valid config, and the defaults live in code
--   ('Heos.Device.Parameters'), not here.
data Config = Config
  { server :: Maybe ServerConfig
  , device :: Maybe DeviceConfig
  , account :: Maybe Credentials
  -- ^ the HEOS account for sign_in; without it streaming services are off
  , logLevel :: Maybe Severity
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
  { address :: Maybe Address
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
