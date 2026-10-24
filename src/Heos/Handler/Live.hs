module Heos.Handler.Live where

import Control.Exception (Exception, throwIO)
import Control.Monad (void, (<=<))
import Data.Aeson (FromJSON (parseJSON))
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, IOE, MonadIO (liftIO), (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Heos.Command (Command (..))
import Heos.Connection (Connection, connectionParams, getPid, sendCommand)
import Heos.Effect (Heos (..))
import Heos.Envelope (Envelope (..), HeosHeader (..))
import Heos.Media (Playable (..), criterionFor, toFound)
import Heos.Player (parsePlayState)
import Heos.Url (decodeQuery, lookupParam)
import Text.Read (readMaybe)

newtype ProtocolError = ProtocolError Text deriving (Show)
instance Exception ProtocolError

runHeosLife :: (IOE :> es) => Connection -> Eff (Heos : es) a -> Eff es a
runHeosLife conn = interpret_ $ \case
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
  pid = getPid . connectionParams $ conn
  call = either throwIO pure <=< sendCommand conn
  vCall = void . call
  orProtocol msg = maybe (throwIO (ProtocolError msg)) pure

-- the payload of a reply, decoded into whatever the operation returns
decodePayload :: (FromJSON a) => Envelope -> IO a
decodePayload env = case env.payload of
  Nothing -> throwIO (ProtocolError "reply without payload")
  Just v -> either (throwIO . ProtocolError . T.pack) pure (parseEither parseJSON v)

-- a value from the reply's message: "pid=…&key=15" -> 15
fromMessage :: Text -> (Text -> Maybe a) -> Envelope -> IO a
fromMessage key parse env =
  maybe (throwIO (ProtocolError ("no " <> key <> " in " <> T.pack (show env.heos.message)))) pure $
    env.heos.message >>= lookupParam key . decodeQuery >>= parse
