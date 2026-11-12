module Heos.Connection.Reconnect where

import Control.Exception (Exception (displayException), throwIO)
import Control.Monad.Catch (Handler (..))
import Control.Retry (recovering)
import Data.List.NonEmpty qualified as NE
import Heos.Connection.Heos (withConnection)
import Heos.Connection.Types
import Heos.Discovery (discover)
import Heos.Error (ConnectionError, DiscoveryError)
import Network.Socket (HostAddress)

-- | Find the device and run the action over a connection to it. If the
--   search fails or the connection drops, wait and start over, search
--   included; the action runs again on every new connection. Returns when
--   the action returns.
withReconnect :: HeosConnectionParams -> (HeosConnection -> IO a) -> IO a
withReconnect params = reconnecting params findAddr
 where
  findAddr = discover >>= either throwIO (pure . NE.head)

reconnecting :: HeosConnectionParams -> IO HostAddress -> (HeosConnection -> IO a) -> IO a
reconnecting params findAddr act = recovering params.retryPolicy handlers $ \_ -> findAddr >>= withConnection params act
 where
  -- подключаться повторно нужно только в случае если это ошибка соединения или обнаружения. Все другие типы ошибок не требуют переподключения
  handlers =
    [ const $ Handler $ \(e :: DiscoveryError) -> True <$ putStrLn ("reconnect: " <> displayException e)
    , const $ Handler $ \(e :: ConnectionError) -> True <$ putStrLn ("reconnect: " <> displayException e)
    ]
