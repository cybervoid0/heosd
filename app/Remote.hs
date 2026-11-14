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
    ('s' : rest) -> searchTracks (trim rest) >>= liftIO . mapM_ (printTable . foundToRow)
    ('f' : 'p' : rest) -> do
      found <- findOne (trim rest)
      liftIO . printTable . foundToRow $ found
      playItem found.target
    line -> fromMaybe (liftIO $ putStrLn help) (lookup line keys)
  foundToRow f = (Just f.name, f.artist, f.album)
  guarded act =
    try act >>= \case
      Left (Refused eid text) -> liftIO . putStrLn $ "Device refused: " <> maybe "" T.unpack text <> " (eid " <> maybe "?" show eid <> ")"
      _ -> pure ()
  printTable = putStrLn . T.unpack . parseRow
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
          Just track -> liftIO . printTable . trackToRow $ track
          Nothing -> liftIO . putStrLn $ "The player is idle"
      )
    , ("que", showQueue >>= liftIO . mapM_ (printTable . trackToRow))
    ]
  trackToRow t = (Just t.song, Just t.artist, Just t.album)
  help = "Commands: on ‧ off ‧ > ‧ || ‧ >> ‧ << ‧ + ‧ - ‧ v (volume) ‧ ps (play state) ‧ np (now playing) ‧ s <query> (search) ‧ fp <query> (find & play) ‧ que (tracks queue) ‧ q (quit)"

parseRow :: (Maybe Text, Maybe Text, Maybe Text) -> Text
parseRow (track, artist, album) = T.intercalate " ‧ " . map joinPair $ table
 where
  joinPair (key, val) = key <> ": " <> showMaybe val
  showMaybe = fromMaybe "unknown"
  table =
    [ ("artist", artist)
    , ("track", track)
    , ("album", album)
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
