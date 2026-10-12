module Heos.FirstTalk (firstTalk) where

import Control.Concurrent.Async (concurrently_, race_)
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forever)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as T
import Data.Void (Void)
import Heos.Command (Command (..))
import Heos.Connection (Connection, sendCommand, subscribe, withConnection)
import Heos.Discovery (discover)
import Heos.Envelope (HeosEvent (..))
import Heos.Player (OnOff (On), PlayState (Pause, Play), PlayerId (PlayerId))
import Heos.Url (lookupParam)
import Text.Read (readMaybe)

-- Стенд
firstTalk :: IO ()
firstTalk =
  discover >>= \case
    Left err -> print err
    Right (addr :| _) -> withConnection addr $ \conn -> do
      ch1 <- subscribe conn
      ch2 <- subscribe conn
      sendCommand conn GetPlayers 1 >>= print
      sendCommand conn (RegisterForChangeEvents On) 2 >>= print
      -- ячейка для хранения громкости
      volumeRef <- newTVarIO (Nothing :: Maybe Int)
      race_ (runPrinter ch1 `concurrently_` runVolTracker ch2 volumeRef) (runRemote conn volumeRef)
     where
      runPrinter ch = printer ch `finally` putStrLn "printer: stopped"
      runVolTracker ch ref = volumeTracker ch ref `finally` putStrLn "volume tracker: stopped"
      runRemote conn ref = remote conn ref 3 `finally` putStrLn "remote: stopped"

printer :: TChan HeosEvent -> IO Void
printer ch = forever $ do
  event <- atomically $ readTChan ch
  print event

volumeTracker :: TChan HeosEvent -> TVar (Maybe Int) -> IO Void
volumeTracker ch volumeRef = forever $ do
  event <- atomically $ readTChan ch
  case event of
    HeosEvent "player_volume_changed" params -> do
      let mbVolume = lookupParam "level" params >>= readMaybe . T.unpack
      atomically $ writeTVar volumeRef mbVolume
    _ -> pure ()

remote :: Connection -> TVar (Maybe Int) -> Int -> IO ()
remote conn volumeRef seqId = do
  line <- getLine
  case line of
    "q" -> pure ()
    "v" -> do
      volume <- readTVarIO volumeRef
      putStrLn (maybe "Volume: unknown" (("Volume: " <>) . show) volume)
      remote conn volumeRef seqId
    _ -> do
      case keyToCommand line of
        Just cmd -> sendCommand conn cmd seqId >>= print
        Nothing -> putStrLn "команды: + - play pause q v"
      remote conn volumeRef (seqId + 1)

keyToCommand :: String -> Maybe Command
keyToCommand cmd = case cmd of
  "+" -> Just $ VolumeUp pid 1
  "-" -> Just $ VolumeDown pid 1
  "play" -> Just $ SetPlayState pid Play
  "pause" -> Just $ SetPlayState pid Pause
  _ -> Nothing
 where
  pid = PlayerId (-1245882071)
