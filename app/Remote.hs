module Remote (remote) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, MonadIO (liftIO), runEff, (:>))
import Effectful.Exception (throwIO, try)
import Heos

-- Стенд
remote :: IO ()
remote = do
  putStrLn $ "HEOS remote controller\n" <> help
  withDevice defaultParams $ \dev -> runEff . runHeosLive dev . runAvrLive dev.avr $ go
 where
  go =
    liftIO getLine >>= \case
      "q" -> pure ()
      line -> guarded (run line) >> go
  run = \case
    ('s' : rest) -> searchTracks (trim rest) >>= liftIO . mapM_ printFound
    ('f' : 'p' : rest) -> do
      found <- findOne (trim rest)
      liftIO . printFound $ found
      playItem found.target
    line -> fromMaybe (liftIO $ putStrLn help) (lookup line keys)
  guarded act =
    try act >>= \case
      Left (Refused eid text) -> liftIO . putStrLn $ "Device refused: " <> maybe "" T.unpack text <> " (eid " <> maybe "?" show eid <> ")"
      _ -> pure ()
  printFound = putStrLn . T.unpack . formatFound
  printRes key = liftIO . putStrLn . ((key <> ": ") <>) . show
  keys =
    [ ("off", turnOff)
    , ("on", turnOn)
    , ("+", volumeUp 1)
    , ("-", volumeDown 1)
    , (">", play)
    , ("||", pause)
    , ("[]", stop)
    , (">>", next)
    , ("<<", prev)
    , ("ps", getPlayState >>= printRes "State")
    , ("v", getVolume >>= printRes "Volume")
    ,
      ( "np"
      , getNowPlaying >>= \case
          Just track -> printRes "Playing" track
          Nothing -> liftIO . putStrLn $ "The player is idle"
      )
    , ("ms", getSources >>= printRes "Music Sources")
    , -- TODO: Сделать форматированный вывод как в поиске, или сделать универсальный форматер который принимает не Found а, например, 4 отдельные поля для вывода на экран
      ("que", showQueue >>= printRes "Queue")
    ]
  help = "Commands: on ‧ off ‧ > ‧ || ‧ >> ‧ << ‧ + ‧ - ‧ v (volume) ‧ ps (play state) ‧ np (now playing) ‧ ms (music sources) ‧ s <query> (search) ‧ fp <query> (find & play) ‧ que (tracks queue) ‧ q (quit)"

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
    [] -> throwIO (Unreadable "Search result is empty")
    (track : _) -> pure track

searchTracks :: (Heos :> es) => Text -> Eff es [Found]
searchTracks query = do
  sourceId <-
    getSearchable kind >>= \case
      [] -> throwIO $ Unreadable "No searchable source"
      (found : _) -> pure found
  search sourceId kind query
 where
  kind = TrackKind
