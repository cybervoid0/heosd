module Heos.Connection.Reconnect where

import Control.Exception (Exception (displayException), IOException, throwIO)
import Control.Monad.Catch (Handler (..))
import Control.Retry (RetryPolicyM, capDelay, exponentialBackoff, recovering)
import Data.List.NonEmpty qualified as NE
import Heos.Connection.Internal (Connection, ConnectionError, withConnection)
import Heos.Discovery (DiscoveryError, discover)
import Heos.Time (Seconds (..), toMicros)
import Network.Socket (SockAddr)

-- | Find the device and run the action over a connection to it. If the
--   search fails or the connection drops, wait and start over, search
--   included; the action runs again on every new connection. Returns when
--   the action returns.
withReconnect :: (Connection -> IO a) -> IO a
withReconnect = reconnecting policy findAddr
 where
  policy = (capDelay . toMicros) (Seconds 60) $ (exponentialBackoff . toMicros) (Seconds 1)
  findAddr = discover >>= either throwIO (pure . NE.head)

reconnecting :: RetryPolicyM IO -> IO SockAddr -> (Connection -> IO a) -> IO a
reconnecting policy findAddr act = recovering policy handlers $ \_ -> findAddr >>= (`withConnection` act)
 where
  handlers =
    [ const $ Handler $ \(e :: DiscoveryError) -> True <$ putStrLn ("reconnect: " <> displayException e)
    , const $ Handler $ \(e :: ConnectionError) -> True <$ putStrLn ("reconnect: " <> displayException e)
    , const $ Handler $ \(e :: IOException) -> True <$ putStrLn ("reconnect: " <> displayException e)
    ]