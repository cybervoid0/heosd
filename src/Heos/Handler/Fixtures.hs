-- | Заготовки для подделки: то, что аппарат «знает» без сети.
-- Треки взяты из записанного ответа @player/get_queue@
-- (@test/fixtures/responses/get_queue.json@), первые десять, с теми же @mid@
-- и @album_id@, что на живом аппарате. Полей @_type@, @station@ и @sid@ в
-- ответе очереди нет, они заполнены как у играющего трека Tidal.
module Heos.Handler.Fixtures (defaultPlayer, dummyQueue, dummySources) where

import Data.Text (Text)
import Data.Text qualified as T
import Heos.Types

defaultPlayer :: Player
defaultPlayer =
  Player
    { name = "Marantz MODEL 60n"
    , pid = PlayerId (-1245882071)
    , model = "Marantz MODEL 60n"
    , version = "3.139.173"
    , ip = "192.168.1.241"
    , network = WiFi
    , lineout = 0
    , serial = "MCQJ072400069"
    }

-- | Очередь из десяти треков, как после @add_container_to_queue@ двух альбомов.
dummyQueue :: [Track]
dummyQueue =
  zipWith
    track
    [1 ..]
    [ ("Heaven's Gonna Burn Your Eyes", richestMan, "496328267")
    , ("Facing East", richestMan, "496328268")
    , ("The Outernationalist", richestMan, "496328269")
    , ("Interlude", richestMan, "496328270")
    , ("Omid", richestMan, "496328271")
    , ("All That We Perceive", richestMan, "496328272")
    , ("Treasures", mirrorConspiracy, "496097051")
    , ("Lebanese Blonde", mirrorConspiracy, "496097054")
    , ("Focus On Sight", mirrorConspiracy, "496097055")
    , ("Air Batucada", mirrorConspiracy, "496097056")
    ]
 where
  track qid (song, (album, album_id, image_url), mid) =
    Track
      { _type = "song"
      , song
      , station = Nothing
      , album
      , artist = "Thievery Corporation"
      , image_url
      , album_id
      , mid = MediaId mid
      , qid = QueueId qid
      , sid = SourceId 10 -- Tidal
      }

-- | Альбом: название, @album_id@, обложка.
type Album = (Text, Text, Text)

richestMan, mirrorConspiracy :: Album
richestMan =
  ( "The Richest Man In Babylon"
  , "496328266"
  , "http://resources.wimpmusic.com/images/382cd18f/1de9/4eda/b70d/2370b985ecd9/640x640.jpg"
  )
mirrorConspiracy =
  ( "Mirror Conspiracy"
  , "496097050"
  , "http://resources.wimpmusic.com/images/b691a44f/50b5/4ee0/a1e0/2f6080a88c60/640x640.jpg"
  )

-- | Источники музыки, как в записанном ответе @browse/get_music_sources@:
-- Tidal подключён, остальные сервисы нет, встроенные источники есть всегда.
-- Имя учётной записи заменено на выдуманное.
dummySources :: [MusicSource]
dummySources =
  [ service "TuneIn" 3 False
  , service "Deezer" 5 False
  , (service "Tidal" 10 True){service_username = Just "listener@example.com"}
  , service "SoundCloud" 9 False
  , service "Qobuz" 30 False
  , builtin "Local Music" "heos_server" 1024
  , builtin "Playlists" "heos_service" 1025
  , builtin "History" "heos_service" 1026
  , builtin "AUX Input" "heos_service" 1027
  , builtin "Favorites" "heos_service" 1028
  ]
 where
  service name = source name "music_service"
  builtin name kind sid = source name kind sid True
  source name kind sid on =
    MusicSource
      { name
      , image_url = "https://production.ws.skyegloup.com:443/media/images/service/logos/" <> logo name
      , _type = kind
      , sid = SourceId sid
      , available = TrueFalse on
      , service_username = Nothing
      }
  logo = \case
    "Local Music" -> "musicsource_logo_servers.png"
    "AUX Input" -> "musicsource_logo_aux.png"
    name -> T.toLower (T.replace " " "" name) <> ".png"
