module Heos.Handler.Fake where

import Control.Monad (when)
import Data.Function (on)
import Data.List (nubBy)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff)
import Effectful.Dispatch.Dynamic (reinterpret_)
import Effectful.Exception (throwIO)
import Effectful.State.Static.Local (evalState, gets, modify)
import Heos.Effect (Heos (..), ProtocolError (ProtocolError))
import Heos.Handler.Fixtures (defaultPlayer, dummyQueue, dummySources)
import Heos.Types

{- handler backed by recorded responses -}
{-
Recorded amplifier responses, no network at all. This is what you debug the
logic against at night without waking the house. Consider a third, recording
handler as well: it proxies to the real device and saves the exchange to disk,
so a day of real use turns into a set of test scenarios from life.
-}

data FakeState = FakeState
  { volume :: Int
  , currentTrack :: Int
  , queue :: [Track]
  , playState :: PlayState
  }
  deriving (Eq, Show)

defaultFakeState :: FakeState
defaultFakeState =
  FakeState
    { volume = 10
    , currentTrack = 0
    , queue = dummyQueue
    , playState = Stop
    }

runHeosFake :: Eff (Heos : es) a -> Eff es a
runHeosFake = reinterpret_ (evalState defaultFakeState) $ \case
  GetPlayersEff -> pure [defaultPlayer]
  GetSourcesEff -> pure dummySources
  GetNowPlayingEff -> do
    queue <- gets @FakeState (.queue)
    when (null queue) $ throwIO (ProtocolError "queue is empty")
    currTrack <- gets @FakeState (.currentTrack)
    pure $ queue !! currTrack
  GetVolumeEff -> gets @FakeState (.volume)
  GetPlayStateEff -> gets @FakeState (.playState)
  SetPlayStateEff ps -> modify $ \s -> s{playState = ps}
  SetVolumeEff volume -> modify $ \s -> s{volume}
  SetVolumeUpEff n -> modify $ \s -> s{volume = volClamp (s.volume + n)}
  SetVolumeDownEff n -> modify $ \s -> s{volume = volClamp (s.volume - n)}
  SearchEff sid kind txt -> pure $ search sid txt kind
  EnqueueEff qm playable -> enqueue qm playable
  PlayNextTrackEff -> modify $ \s -> s{currentTrack = min (length s.queue - 1) (s.currentTrack + 1)}
  PlayPrevTrackEff -> modify $ \s -> s{currentTrack = max 0 (s.currentTrack - 1)}
 where
  volClamp = max 0 . min 100
  enqueue qm playable = do
    queue <- gets @FakeState (.queue)
    currTrack <- gets @FakeState (.currentTrack)
    let newTracks = tracksFor playable
        (newCurr, newQueue) = getNewQueue newTracks currTrack queue qm
    modify $ \s -> s{queue = newQueue, currentTrack = newCurr, playState = Play}

tracksFor :: Playable -> [Track]
tracksFor = \case
  PlayTrack _ _ mid -> filter (\t -> t.mid == mid) dummyQueue
  PlayContainer _ (ContainerId cid) -> filter (\t -> t.album_id == cid) dummyQueue

getNewQueue :: [a] -> Int -> [a] -> QueueMode -> (Int, [a])
getNewQueue tracks curr queue = \case
  AddPlayNow -> let (a, b) = splitAt curr queue in (curr, a <> tracks <> b)
  AddPlayNext -> let (a, b) = splitAt (curr + 1) queue in (curr, a <> tracks <> b)
  AddToEnd -> (curr, queue <> tracks)
  AddReplaceAndPlay -> (0, tracks)

-- | Поиск по каталогу: смотрит в поле, которое соответствует виду поиска,
-- без учёта регистра. Альбомы и исполнители схлопываются до одного на имя.
search :: SourceId -> Text -> SearchKind -> [Found]
search sid query kind = case kind of
  TrackKind -> map foundTrack (matching (.song))
  AlbumKind -> map foundAlbum (nubBy ((==) `on` (.album_id)) (matching (.album)))
  ArtistKind -> map foundArtist (nubBy ((==) `on` (.artist)) (matching (.artist)))
  PlaylistKind -> [] -- плейлистов в каталоге нет
 where
  matching field = filter (\t -> T.toLower query `T.isInfixOf` T.toLower (field t)) dummyQueue
  foundTrack t =
    Found
      { name = t.song
      , artist = Just t.artist
      , album = Just t.album
      , kind = "song"
      , target = Just (PlayTrack sid (ContainerId ("SEARCHED_TRACKS-" <> query)) t.mid)
      }
  foundAlbum t =
    Found
      { name = t.album
      , artist = Just t.artist
      , album = Nothing
      , kind = "album"
      , target = Just (PlayContainer sid (ContainerId t.album_id)) -- в подделке cid альбома = album_id
      }
  foundArtist t =
    Found
      { name = t.artist
      , artist = Nothing
      , album = Nothing
      , kind = "artist"
      , target = Nothing -- исполнителя целиком подделка не играет
      }
