module Remote (remote) where

import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Effectful (MonadIO (liftIO), runEff)
import Heos

-- Стенд
remote :: IO ()
remote = do
  putStrLn $ "HEOS remote controller\n" <> help
  withDevice defaultParams $ \dev -> runEff . runHeosLive dev . runAvrLive dev.avr $ go
 where
  go =
    liftIO (T.strip . T.pack <$> getLine) >>= \case
      "q" -> pure ()
      line -> fromMaybe (liftIO $ putStrLn help) (lookup line keys) >> go
  keys =
    [ ("off", setPower Off)
    , ("on", setPower On)
    , ("+", volumeUp 1)
    , ("-", volumeDown 1)
    , (">", setPlayState Play)
    , ("||", setPlayState Pause)
    , (">>", playNext)
    , ("<<", playPrevious)
    , ("ps", getPlayState >>= liftIO . putStrLn . ("State: " <>) . show)
    , ("v", getVolume >>= liftIO . putStrLn . ("Volume: " <>) . show)
    ,
      ( "np"
      , getNowPlaying >>= \case
          Just track -> liftIO . putStrLn . ("Playing: " <>) . show $ track
          Nothing -> liftIO . putStrLn $ "The player is idle"
      )
    , ("ms", getSources >>= liftIO . putStrLn . ("Music Sources: " <>) . show)
    ]
  help = "Commands: on ‧ off ‧ > ‧ || ‧ >> ‧ << ‧ + ‧ - ‧ v (volume) ‧ ps (play state) ‧ np (now playing) ‧ ms (music sources) ‧ q (quit)"
