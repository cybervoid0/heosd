module Remote (remote) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, MonadIO (liftIO), runEff, (:>))
import Effectful.Exception (throwIO)
import Heos

-- Стенд
remote :: IO ()
remote = do
  putStrLn $ "HEOS remote controller\n" <> help
  withDevice defaultParams $ \dev -> runEff . runHeosLive dev . runAvrLive dev.avr $ go
 where
  go =
    liftIO getLine >>= \case
      -- quit
      "q" -> pure ()
      -- search
      ('s' : rest) -> searchTracks (trim rest) >>= liftIO . mapM_ printFound >> go
      -- find and play
      ('f' : 'p' : rest) -> do
        found <- findOne (trim rest)
        liftIO . printFound $ found
        playItem found.target
        go
      line -> fromMaybe (liftIO $ putStrLn help) (lookup line keys) >> go
  printFound = putStrLn . T.unpack . formatFound
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
  help = "Commands: on ‧ off ‧ > ‧ || ‧ >> ‧ << ‧ + ‧ - ‧ v (volume) ‧ ps (play state) ‧ np (now playing) ‧ ms (music sources) ‧ s <query> (search) ‧ fp (find & play) ‧ q (quit)"

formatFound :: Found -> Text
formatFound f = T.intercalate " ‧ " . map joinPair $ table
 where
  joinPair (key, val) = key <> ": " <> val
  showMaybe = fromMaybe "unknown"
  table =
    [ ("artist", showMaybe f.artist)
    , ("track", f.name)
    , ("album", showMaybe f.album)
    ]

trim :: String -> Text
trim = T.strip . T.pack

findOne :: (Heos :> es) => Text -> Eff es Found
findOne query =
  searchTracks query >>= \case
    [] -> throwIO (ProtocolError "Search result is empty")
    (track : _) -> pure track

searchTracks :: (Heos :> es) => Text -> Eff es [Found]
searchTracks query = do
  sourceId <-
    getSearchable kind >>= \case
      [] -> throwIO $ ProtocolError "No searchable source"
      (found : _) -> pure found
  search sourceId kind query
 where
  kind = TrackKind
