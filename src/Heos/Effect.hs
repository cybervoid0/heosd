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
  Heos (..),
  ProtocolError (..),
) where

import Control.Exception (Exception)
import Control.Monad (forM_)
import Data.Text (Text)
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (send)
import Heos.Types

newtype ProtocolError = ProtocolError Text deriving (Show)
instance Exception ProtocolError

{- the Heos effect: domain operations -}

data Heos :: Effect where
  GetPlayersEff :: Heos m [Player]
  GetSourcesEff :: Heos m [MusicSource]
  GetNowPlayingEff :: Heos m Track
  GetVolumeEff :: Heos m Int
  GetPlayStateEff :: Heos m PlayState
  SetPlayStateEff :: PlayState -> Heos m ()
  SetVolumeEff :: Int -> Heos m ()
  SetVolumeUpEff :: Int -> Heos m ()
  SetVolumeDownEff :: Int -> Heos m ()
  SearchEff :: SourceId -> SearchKind -> Text -> Heos m [Found]
  EnqueueEff :: QueueMode -> Playable -> Heos m ()
  PlayNextTrackEff :: Heos m ()
  PlayPrevTrackEff :: Heos m ()

type instance DispatchOf Heos = Dynamic

getPlayers :: (Heos :> es) => Eff es [Player]
getPlayers = send GetPlayersEff

getSources :: (Heos :> es) => Eff es [MusicSource]
getSources = send GetSourcesEff

getNowPlaying :: (Heos :> es) => Eff es Track
getNowPlaying = send GetNowPlayingEff

getVolume :: (Heos :> es) => Eff es Int
getVolume = send GetVolumeEff

getPlayState :: (Heos :> es) => Eff es PlayState
getPlayState = send GetPlayStateEff
setPlayState :: (Heos :> es) => PlayState -> Eff es ()
setPlayState = send . SetPlayStateEff

setVolume :: (Heos :> es) => Int -> Eff es ()
setVolume = send . SetVolumeEff

volumeUp :: (Heos :> es) => Int -> Eff es ()
volumeUp = send . SetVolumeUpEff

volumeDown :: (Heos :> es) => Int -> Eff es ()
volumeDown = send . SetVolumeDownEff

search :: (Heos :> es) => SourceId -> SearchKind -> Text -> Eff es [Found]
search sid sk = send . SearchEff sid sk

enqueue :: (Heos :> es) => QueueMode -> Playable -> Eff es ()
enqueue qm = send . EnqueueEff qm

playItem :: (Heos :> es) => Playable -> Eff es ()
playItem = enqueue AddReplaceAndPlay

playAll :: (Heos :> es) => [Playable] -> Eff es ()
playAll [] = pure ()
playAll (x : xs) = playItem x >> forM_ xs (enqueue AddToEnd)

playNext :: (Heos :> es) => Eff es ()
playNext = send PlayNextTrackEff

playPrevious :: (Heos :> es) => Eff es ()
playPrevious = send PlayPrevTrackEff
