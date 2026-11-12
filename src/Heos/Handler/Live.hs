module Heos.Handler.Live where

import Control.Concurrent.STM (readTVarIO)
import Control.Exception (throwIO)
import Control.Monad (void)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Effectful (Eff, IOE, MonadIO (liftIO), (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Heos.Command.Heos (Command (..))
import Heos.Connection (HeosConnection (..), sendCommand)
import Heos.Device (Catalogue (..), Device (..), LiveState (..), searchable)
import Heos.Effect.Heos (Heos (..))
import Heos.Error (CommandError (..))
import Heos.Types

runHeosLive :: (IOE :> es) => Device -> Eff (Heos : es) a -> Eff es a
runHeosLive device = interpret_ $ \case
  EGetPlayers -> pure device.catalogue.players
  EGetSources -> pure device.catalogue.sources
  EGetNowPlaying -> liftIO $ readState (.nowPlaying)
  EGetVolume -> liftIO $ readState (.volume)
  EGetPlayState -> liftIO $ readState (.playState)
  ESetPlayState ps -> liftIO $ vCall (SetPlayState pid ps)
  ESetVolume vol -> liftIO $ vCall (SetVolume pid vol)
  ESetVolumeUp n -> liftIO $ vCall (VolumeUp pid n)
  ESetVolumeDown n -> liftIO $ vCall (VolumeDown pid n)
  EMute -> liftIO $ vCall (ToggleMute pid)
  ESearchable kind -> pure . searchable kind $ device.catalogue
  ESearch sid kind txt -> liftIO $ do
    criteria <- orProtocol ("unknown source " <> T.pack (show sid)) (Map.lookup sid device.catalogue.criteria)
    scid <- orProtocol ("source has no " <> T.pack (show kind) <> " search") (criterionFor kind criteria)
    results <- call (Search sid txt scid Nothing)
    pure . mapMaybe (toFound sid) $ results
  EEnqueue qm playable -> liftIO . vCall $ case playable of
    PlayTrack sid cid mid -> AddTrackToQueue pid sid cid mid qm
    PlayContainer sid cid -> AddContainerToQueue pid sid cid qm
  -- `Nothing` means returning all elements of the queue (up to 100)
  EShowQueue -> liftIO $ call (GetQueue pid Nothing)
  EClearQueue -> liftIO $ vCall (ClearQueue pid)
  EPlayNextTrack -> liftIO $ vCall (PlayNext pid)
  EPlayPrevTrack -> liftIO $ vCall (PlayPrevious pid)
 where
  readState :: (LiveState -> a) -> IO a
  readState = (<$> readTVarIO device.state)
  pid = device.heos.pid
  call :: Command a -> IO a
  call = sendCommand device.heos
  vCall = void . call
  orProtocol msg = maybe (throwIO (Unreadable msg)) pure
