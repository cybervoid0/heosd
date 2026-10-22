module Heos.Effect (
  getPlayers,
  getSources,
  getNowPlaying,
  getVolume,
  getPlayState,
  setPlayState,
  setVolume,
  volumeUp,
  volumeDown,
  search,
  enqueue,
  playItem,
  playAll,
  playNext,
  playPrevious,
) where

import Control.Monad (forM_)
import Data.Text (Text)
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (send)
import Heos.Command (QueueMode (AddReplaceAndPlay, AddToEnd))
import Heos.Media (ContainerId, MediaId, MusicSource, SearchKind, SourceId)
import Heos.Player (NowPlaying, PlayState, Player)

{- the Heos effect: domain operations -}

data Playable
  = PlayTrack SourceId ContainerId MediaId
  | PlayContainer SourceId ContainerId
  deriving (Show, Eq)

data Found = Found
  { name :: Text
  , artist :: Maybe Text
  , album :: Maybe Text
  , kind :: Text
  , target :: Maybe Playable -- Nothing, если playable = no
  }
  deriving (Show, Eq)

data Heos :: Effect where
  GetPlayers :: Heos m [Player]
  GetSources :: Heos m [MusicSource]
  GetNowPlaying :: Heos m NowPlaying
  GetVolume :: Heos m Int
  GetPlayState :: Heos m PlayState
  SetPlayState :: PlayState -> Heos m ()
  SetVolume :: Int -> Heos m ()
  SetVolumeUp :: Int -> Heos m ()
  SetVolumeDown :: Int -> Heos m ()
  Search :: SearchKind -> Text -> Heos m [Found]
  Enqueue :: QueueMode -> Playable -> Heos m ()
  PlayNextTrack :: Heos m ()
  PlayPrevTrack :: Heos m ()

type instance DispatchOf Heos = Dynamic

getPlayers :: (Heos :> es) => Eff es [Player]
getPlayers = send GetPlayers

getSources :: (Heos :> es) => Eff es [MusicSource]
getSources = send GetSources

getNowPlaying :: (Heos :> es) => Eff es NowPlaying
getNowPlaying = send GetNowPlaying

getVolume :: (Heos :> es) => Eff es Int
getVolume = send GetVolume

getPlayState :: (Heos :> es) => Eff es PlayState
getPlayState = send GetPlayState
setPlayState :: (Heos :> es) => PlayState -> Eff es ()
setPlayState = send . SetPlayState

setVolume :: (Heos :> es) => Int -> Eff es ()
setVolume = send . SetVolume

volumeUp :: (Heos :> es) => Int -> Eff es ()
volumeUp = send . SetVolumeUp

volumeDown :: (Heos :> es) => Int -> Eff es ()
volumeDown = send . SetVolumeDown

search :: (Heos :> es) => SearchKind -> Text -> Eff es [Found]
search sk = send . Search sk

enqueue :: (Heos :> es) => QueueMode -> Playable -> Eff es ()
enqueue qm = send . Enqueue qm

playItem :: (Heos :> es) => Playable -> Eff es ()
playItem = enqueue AddReplaceAndPlay

playAll :: (Heos :> es) => [Playable] -> Eff es ()
playAll [] = pure ()
playAll (x : xs) = playItem x >> forM_ xs (enqueue AddToEnd)

playNext :: (Heos :> es) => Eff es ()
playNext = send PlayNextTrack

playPrevious :: (Heos :> es) => Eff es ()
playPrevious = send PlayPrevTrack
