module Heos.Logger where

import Control.Concurrent (newMVar, withMVar)
import Control.Exception (Exception (displayException), SomeException, throwIO, try)
import Control.Monad (when)
import Data.Aeson (FromJSON (..), withText)
import Data.Function ((&))
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Data.Text.IO (hPutStrLn)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import GHC.Clock (getMonotonicTime)
import System.IO (Handle)

data Severity = Debug | Info | Warn | Error deriving (Eq, Ord, Show)
instance FromJSON Severity where
  parseJSON = withText "LogLevel" $ \case
    "debug" -> pure Debug
    "info" -> pure Info
    "warn" -> pure Warn
    "error" -> pure Error
    other -> fail ("unknown log level: " <> show other <> ", expected debug, info, warn or error")

newtype Logger = Logger {logAt :: Severity -> String -> IO ()}
  deriving newtype (Semigroup, Monoid)

newLogger :: Handle -> Severity -> IO Logger
newLogger h minSev = do
  lock <- newMVar ()
  pure . Logger $ \sev msg -> when (sev >= minSev) $ do
    now <- getCurrentTime
    let stamp = now & formatTime defaultTimeLocale "%F %T" & T.pack
        severity = sev & show & T.pack & T.toUpper
        message = T.pack msg
        line = T.unwords [stamp, severity, message]
    withMVar lock $ \_ -> hPutStrLn h line

scoped :: String -> Logger -> Logger
scoped name inner = Logger $ \sev msg -> inner.logAt sev (name <> ": " <> msg)

-- | Measures the execution time of the function. Returns ms
timed :: IO a -> IO (a, Int)
timed action = do
  t0 <- getMonotonicTime -- GHC.Clock, seconds
  result <- action
  t1 <- getMonotonicTime
  let t = t1 - t0
  pure (result, round (t * 1000))

logged :: Logger -> String -> IO a -> IO a
logged logger msg act = do
  (reply, time) <- timed (try @SomeException act)
  case reply of
    Left e -> logAt logger Warn $ msg <> " has failed with: " <> displayException e <> " in " <> show time <> " ms"
    Right _ -> logAt logger Debug $ msg <> " executed in " <> show time <> " ms"
  either throwIO pure reply

capturing :: IO (Logger, IO [(Severity, String)])
capturing = do
  ref <- newIORef []
  let write sev msg = atomicModifyIORef' ref (\seen -> ((sev, msg) : seen, ()))
  pure (Logger write, reverse <$> readIORef ref)
