module FirstTalk (firstTalk) where

import Control.Concurrent.Async (concurrently_, race_)
import Control.Concurrent.STM
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forever)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Void (Void)
import Effectful (Eff, IOE, MonadIO (liftIO), runEff, (:>))
import Heos
import Text.Read (readMaybe)

-- Стенд
firstTalk :: IO ()
firstTalk = withReconnect $ \conn -> do
  printChan <- subscribe conn
  volChan <- subscribe conn
  volumeRef <- newTVarIO (Nothing :: Maybe Int)
  race_ (printer printChan `concurrently_` volumeTracker volChan volumeRef) (runEff $ runHeosLife conn (remote volumeRef))

printer :: TChan HeosEvent -> IO Void
printer ch = forever $ atomically (readTChan ch) >>= print

volumeTracker :: TChan HeosEvent -> TVar (Maybe Int) -> IO Void
volumeTracker ch volumeRef =
  forever $
    atomically (readTChan ch) >>= \case
      HeosEvent "player_volume_changed" params -> do
        -- вычислить число до записи: иначе в TVar лежал бы thunk, держащий событие
        volume <- evaluate . force $ lookupParam "level" params >>= readMaybe . T.unpack
        atomically $ writeTVar volumeRef volume
      _ -> pure ()

remote :: (Heos :> es, IOE :> es) => TVar (Maybe Int) -> Eff es ()
remote volumeRef =
  liftIO getLine >>= \case
    "q" -> pure ()
    "v" -> do
      volume <- liftIO $ readTVarIO volumeRef
      liftIO $ putStrLn (maybe "Volume: unknown" (("Volume: " <>) . show) volume)
      remote volumeRef
    line -> fromMaybe (liftIO $ putStrLn "команды: + - > || >> << q v") (keyToCommand line) >> remote volumeRef
 where
  keyToCommand = \case
    "+" -> Just $ volumeUp 1
    "-" -> Just $ volumeDown 1
    ">" -> Just $ setPlayState Play
    "||" -> Just $ setPlayState Pause
    ">>" -> Just playNext
    "<<" -> Just playPrevious
    _ -> Nothing
