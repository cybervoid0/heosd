module Remote (remote) where

import Data.Maybe (fromMaybe)
import Effectful (MonadIO (liftIO), runEff)
import Heos

-- Стенд
remote :: IO ()
remote = withDevice defaultParams $ \dev -> runEff . runHeosLive dev . runAvrLive dev $ go
 where
  go =
    liftIO getLine >>= \case
      "q" -> pure ()
      line -> fromMaybe (liftIO $ putStrLn "команды: + - > || >> << q v on off") (lookup line keys) >> go
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
