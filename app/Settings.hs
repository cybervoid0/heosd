module Settings (loadParams) where

import Config (Address (..), Config (..), DeviceConfig (..), RetryConfig (..))
import Control.Applicative ((<|>))
import Control.Retry (capDelay, exponentialBackoff)
import Data.Functor ((<&>))
import Data.IP (toHostAddress)
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Heos (DeviceParams (..), HeosConnectionParams (..), Seconds (Seconds), defaultParams, toMicros)
import System.Directory (XdgDirectory (XdgConfig), doesFileExist, getXdgDirectory)
import System.Exit (die)
import System.FilePath ((</>))

dp :: DeviceParams
dp = defaultParams

configPath :: IO FilePath
configPath = (</> "config.yaml") <$> getXdgDirectory XdgConfig "heosd"

loadConfig :: FilePath -> IO Config
loadConfig fp = do
  exists <- doesFileExist fp
  if not exists
    then pure $ Config Nothing Nothing Nothing Nothing
    else decodeFileEither fp >>= either (die . prettyPrintParseException) pure

fromConfig :: Config -> DeviceParams
fromConfig cfg =
  dp
    { heos =
        dp.heos
          { commandTimeout = maybe dp.heos.commandTimeout Seconds (dev >>= (.commandTimeout))
          , heartbeatDelay = maybe dp.heos.heartbeatDelay Seconds (dev >>= (.heartbeatDelay))
          , retryPolicy = maybe dp.heos.retryPolicy getRetryPolicy (dev >>= (.retry))
          , account = cfg.account <|> dp.heos.account
          , address = ((dev >>= (.address)) <&> toHostAddress . (.ipv4)) <|> dp.heos.address
          }
    }
 where
  dev = cfg.device
  getRetryPolicy (RetryConfig base cap) = (capDelay . toMicros) (Seconds cap) $ (exponentialBackoff . toMicros) (Seconds base)

loadParams :: IO DeviceParams
loadParams = fromConfig <$> (configPath >>= loadConfig)
