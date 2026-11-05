module FirstTalk (firstTalk) where

import Control.Concurrent.Async (race_)
import Control.Concurrent.STM
import Control.Monad (forever)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Effectful (Eff, IOE, MonadIO (liftIO), runEff, (:>))
import Heos

-- Стенд
firstTalk :: IO ()
firstTalk = withDevice defaultParams $ \dev -> do
  printChan <- subscribe dev.heos
  let eff = runEff . runHeosLive dev . runAvrLive dev.avr
  race_ (printer printChan) (eff remote)

printer :: TChan HeosEvent -> IO Void
printer ch = forever $ atomically (readTChan ch) >>= print

remote :: (Heos :> es, IOE :> es, Avr :> es) => Eff es ()
remote =
  liftIO getLine >>= \case
    "q" -> pure ()
    line -> fromMaybe (liftIO $ putStrLn "команды: + - > || >> << q v on off") (lookup line keys) >> remote

keys :: (Heos :> es, Avr :> es, IOE :> es) => [(String, Eff es ())]
keys =
  [ ("off", setPower Off)
  , ("on", setPower On)
  , ("+", volumeUp 1)
  , ("-", volumeDown 1)
  , (">", setPlayState Play)
  , ("||", setPlayState Pause)
  , (">>", playNext)
  , ("<<", playPrevious)
  , ("ps", getPlayState >>= liftIO . print)
  , ("v", getVolume >>= liftIO . putStrLn . ("Volume: " <>) . show)
  ,
    ( "np"
    , getNowPlaying >>= \case
        Just track -> liftIO . putStrLn . ("Playing: " <>) . show $ track
        Nothing -> liftIO . putStrLn $ "The player is idle"
    )
  , ("ms", getSources >>= liftIO . putStrLn . ("Music Sources: " <>) . show)
  ]
