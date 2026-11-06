module Heos.Effect.Heos where

import Control.Monad (forM_)
import Data.Text (Text)
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (send)
import Heos.Types

{- the Heos effect: domain operations -}

data Heos :: Effect where
  EGetPlayers :: Heos m [Player]
  EGetSources :: Heos m [MusicSource]
  EGetNowPlaying :: Heos m (Maybe Track)
  EGetVolume :: Heos m Int
  EGetPlayState :: Heos m PlayState
  ESetPlayState :: PlayState -> Heos m ()
  ESetVolume :: Int -> Heos m ()
  ESetVolumeUp :: Int -> Heos m ()
  ESetVolumeDown :: Int -> Heos m ()
  ESearchable :: SearchKind -> Heos m [SourceId]
  ESearch :: SourceId -> SearchKind -> Text -> Heos m [Found]
  EEnqueue :: QueueMode -> Playable -> Heos m ()
  EPlayNextTrack :: Heos m ()
  EPlayPrevTrack :: Heos m ()

type instance DispatchOf Heos = Dynamic

getPlayers :: (Heos :> es) => Eff es [Player]
getPlayers = send EGetPlayers

getSources :: (Heos :> es) => Eff es [MusicSource]
getSources = send EGetSources

getNowPlaying :: (Heos :> es) => Eff es (Maybe Track)
getNowPlaying = send EGetNowPlaying

getVolume :: (Heos :> es) => Eff es Int
getVolume = send EGetVolume

getPlayState :: (Heos :> es) => Eff es PlayState
getPlayState = send EGetPlayState
setPlayState :: (Heos :> es) => PlayState -> Eff es ()
setPlayState = send . ESetPlayState

setVolume :: (Heos :> es) => Int -> Eff es ()
setVolume = send . ESetVolume

volumeUp :: (Heos :> es) => Int -> Eff es ()
volumeUp = send . ESetVolumeUp

volumeDown :: (Heos :> es) => Int -> Eff es ()
volumeDown = send . ESetVolumeDown

getSearchable :: (Heos :> es) => SearchKind -> Eff es [SourceId]
getSearchable = send . ESearchable

search :: (Heos :> es) => SourceId -> SearchKind -> Text -> Eff es [Found]
search sid sk = send . ESearch sid sk

enqueue :: (Heos :> es) => QueueMode -> Playable -> Eff es ()
enqueue qm = send . EEnqueue qm

playItem :: (Heos :> es) => Playable -> Eff es ()
playItem = enqueue AddReplaceAndPlay

playAll :: (Heos :> es) => [Playable] -> Eff es ()
playAll [] = pure ()
playAll (x : xs) = playItem x >> forM_ xs (enqueue AddToEnd)

playNext :: (Heos :> es) => Eff es ()
playNext = send EPlayNextTrack

playPrevious :: (Heos :> es) => Eff es ()
playPrevious = send EPlayPrevTrack
