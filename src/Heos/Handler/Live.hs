module Heos.Handler.Live where

import Control.Exception (throwIO)
import Control.Monad (void, (<=<))
import Data.Text qualified as T
import Effectful (Eff, IOE, MonadIO (liftIO), (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Heos.Command.Heos (Command (..))
import Heos.Connection (HeosConnection (..), sendCommand)
import Heos.Effect.Heos (Heos (..))
import Heos.Envelope.Heos (decodePayload, fromMessage)
import Heos.Types
import Text.Read (readMaybe)

runHeosLive :: (IOE :> es) => HeosConnection -> Eff (Heos : es) a -> Eff es a
runHeosLive conn = interpret_ $ \case
  GetPlayersEff -> liftIO $ call GetPlayers >>= decodePayload
  GetSourcesEff -> liftIO $ call GetMusicSources >>= decodePayload
  GetNowPlayingEff -> liftIO $ call (GetNowPlayingMedia pid) >>= decodePayload
  GetVolumeEff -> liftIO $ call (GetVolume pid) >>= fromMessage "level" (readMaybe . T.unpack)
  GetPlayStateEff -> liftIO $ call (GetPlayState pid) >>= fromMessage "state" parsePlayState
  SetPlayStateEff ps -> liftIO $ vCall (SetPlayState pid ps)
  SetVolumeEff vol -> liftIO $ vCall (SetVolume pid vol)
  SetVolumeUpEff n -> liftIO $ vCall (VolumeUp pid n)
  SetVolumeDownEff n -> liftIO $ vCall (VolumeDown pid n)
  SearchEff sid kind txt -> liftIO $ do
    criteria <- call (GetSearchCriteria sid) >>= decodePayload
    scid <- orProtocol ("source has no " <> T.pack (show kind) <> " search") (criterionFor kind criteria)
    results <- call (Search sid txt scid Nothing) >>= decodePayload
    pure (map (toFound sid txt) results)
  EnqueueEff qm playable -> liftIO . vCall $ case playable of
    PlayTrack sid cid mid -> AddTrackToQueue pid sid cid mid qm
    PlayContainer sid cid -> AddContainerToQueue pid sid cid qm
  PlayNextTrackEff -> liftIO $ vCall (PlayNext pid)
  PlayPrevTrackEff -> liftIO $ vCall (PlayPrevious pid)
 where
  pid = conn.pid
  call = either throwIO pure <=< sendCommand conn
  vCall = void . call
  orProtocol msg = maybe (throwIO (ProtocolError msg)) pure
