module Heos.FirstTalk (firstTalk) where

import Control.Concurrent.Async (concurrently_, race_)
import Control.Concurrent.STM
import Control.Monad (forever)
import Data.Text qualified as T
import Data.Void (Void)
import Heos.Command (Command (..))
import Heos.Connection (Connection, sendCommand, subscribe, withReconnect)
import Heos.Connection.Parameters (ConnectionParams (..), defaultParams)
import Heos.Envelope (HeosEvent (..))
import Heos.Player (PlayState (Pause, Play), PlayerId)
import Heos.Url (lookupParam)
import Text.Read (readMaybe)

-- Стенд
firstTalk :: IO ()
firstTalk = withReconnect $ \conn -> do
  printChan <- subscribe conn
  volChan <- subscribe conn
  volumeRef <- newTVarIO (Nothing :: Maybe Int)
  race_ (printer printChan `concurrently_` volumeTracker volChan volumeRef) (remote conn volumeRef)

printer :: TChan HeosEvent -> IO Void
printer ch = forever $ atomically (readTChan ch) >>= print

volumeTracker :: TChan HeosEvent -> TVar (Maybe Int) -> IO Void
volumeTracker ch volumeRef =
  forever $
    atomically (readTChan ch) >>= \case
      HeosEvent "player_volume_changed" params -> do
        let mbVolume = lookupParam "level" params >>= readMaybe . T.unpack
        atomically $ writeTVar volumeRef mbVolume
      _ -> pure ()

remote :: Connection -> TVar (Maybe Int) -> IO ()
remote conn volumeRef = do
  line <- getLine
  case line of
    "q" -> pure ()
    "v" -> do
      volume <- readTVarIO volumeRef
      putStrLn (maybe "Volume: unknown" (("Volume: " <>) . show) volume)
      remote conn volumeRef
    _ -> do
      case keyToCommand line defaultParams.playerId of
        Just cmd -> sendCommand conn cmd >>= print
        Nothing -> putStrLn "команды: + - > || >> << q v"
      remote conn volumeRef

keyToCommand :: String -> PlayerId -> Maybe Command
keyToCommand cmd pid = case cmd of
  "+" -> Just $ VolumeUp pid 1
  "-" -> Just $ VolumeDown pid 1
  ">" -> Just $ SetPlayState pid Play
  "||" -> Just $ SetPlayState pid Pause
  ">>" -> Just $ PlayNext pid
  "<<" -> Just $ PlayPrevious pid
  _ -> Nothing
