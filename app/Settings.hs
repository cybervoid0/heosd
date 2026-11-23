module Settings (loadParams) where

import Config (Address (..), Config (..), DeviceConfig (..), RetryConfig (..))
import Control.Applicative ((<|>))
import Control.Retry (capDelay, exponentialBackoff)
import Data.Functor ((<&>))
import Data.IP (toHostAddress)
import Data.Maybe (fromMaybe)
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Heos (DeviceParams (..), HeosConnectionParams (..), Seconds (Seconds), defaultParams, toMicros)
import Heos.Connection.Avr qualified as Avr
import Heos.Logger (Logger (..), Severity (..), newLogger)
import System.Directory (XdgDirectory (XdgConfig), doesFileExist, getXdgDirectory)
import System.Exit (die)
import System.FilePath ((</>))
import System.IO (stderr)

dp :: DeviceParams
dp = defaultParams

configPath :: IO FilePath
configPath = (</> "config.yaml") <$> getXdgDirectory XdgConfig "heosd"

data ConfigLocation = DefaultParams | UserDefined FilePath

loadConfig :: FilePath -> IO (Config, ConfigLocation)
loadConfig fp = do
  exists <- doesFileExist fp
  if not exists
    then pure (Config Nothing Nothing Nothing Nothing, DefaultParams)
    else decodeFileEither fp >>= \eitherConfig -> either (die . prettyPrintParseException) (\config -> pure (config, UserDefined fp)) eitherConfig

fromConfig :: Config -> Logger -> DeviceParams
fromConfig cfg logger =
  dp
    { heos =
        dp.heos
          { commandTimeout = maybe dp.heos.commandTimeout Seconds (dev >>= (.commandTimeout))
          , heartbeatDelay = maybe dp.heos.heartbeatDelay Seconds (dev >>= (.heartbeatDelay))
          , retryPolicy = maybe dp.heos.retryPolicy getRetryPolicy (dev >>= (.retry))
          , account = cfg.account <|> dp.heos.account
          , address = ((dev >>= (.address)) <&> toHostAddress . (.ipv4)) <|> dp.heos.address
          , logger
          }
    , avr =
        dp.avr
          { Avr.logger = logger
          }
    }
 where
  dev = cfg.device
  getRetryPolicy (RetryConfig base cap) = (capDelay . toMicros) (Seconds cap) $ (exponentialBackoff . toMicros) (Seconds base)

loadParams :: IO DeviceParams
loadParams = do
  (config, cfgLocation) <- configPath >>= loadConfig
  let logLevel = fromMaybe Info config.logLevel
  logger <- newLogger stderr logLevel
  logConfig logger cfgLocation
  pure $ fromConfig config logger
 where
  logConfig logger location = logger.logAt Info $ case location of
    UserDefined fp -> "Loaded config file: " <> show fp
    DefaultParams -> "Config file not found. Taking default params"