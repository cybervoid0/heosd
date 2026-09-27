module Heos.Player (
  PlayerId (..),
  PlayState (..),
  RepeatMode (..),
  GroupId (..),
  OnOff (..),
) where

import Heos.ToParam (ToParam (..))

newtype PlayerId = PlayerId Int deriving (Show, Eq)
instance ToParam PlayerId where
  toParam (PlayerId pid) = toParam pid

newtype GroupId = GroupId Int deriving (Show, Eq)
instance ToParam GroupId where
  toParam (GroupId gid) = toParam gid

data PlayState = Play | Pause | Stop deriving (Show, Eq)
instance ToParam PlayState where
  toParam = \case
    Play -> "play"
    Pause -> "pause"
    Stop -> "stop"

data RepeatMode = RepeatAll | RepeatOne | RepeatOff deriving (Show, Eq)
instance ToParam RepeatMode where
  toParam = \case
    RepeatAll -> "on_all"
    RepeatOne -> "on_one"
    RepeatOff -> "off"

data OnOff = On | Off deriving (Show, Eq)
instance ToParam OnOff where
  toParam status = case status of
    On -> "on"
    Off -> "off"