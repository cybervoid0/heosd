module Heos.Device where

import Control.Concurrent.Async (race)
import Control.Concurrent.STM (TChan, TVar, atomically, modifyTVar', newTVarIO, readTChan)
import Control.Exception (handle)
import Control.Monad (forM, forM_, forever)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Void (Void, absurd)
import Heos.Command.Heos (Command (..))
import Heos.Connection (HeosConnection (..), getAddr, sendCommand, subscribe, withAvr, withReconnect)
import Heos.Connection.Avr (AvrConnection)
import Heos.Connection.Heos (sendOn)
import Heos.Connection.Types (ConnectionSockets (..))
import Heos.Device.Parameters
import Heos.Envelope.Heos (HeosEvent (HeosEvent), decodePayload, fromMessage)
import Heos.Types
import Heos.Url (lookupParam)
import Text.Read (readMaybe)

data Device = Device
  { heos :: HeosConnection
  , avr :: AvrConnection
  , catalogue :: Catalogue
  , state :: TVar LiveState
  }

withDevice :: DeviceParams -> (Device -> IO a) -> IO a
withDevice params act = withReconnect params.heos $ \conn ->
  withAvr params.avr (getAddr conn) $ \avrConn -> do
    chan <- subscribe conn
    (catalogue, liveState) <- initialize conn
    state <- newTVarIO liveState
    let dev = Device{heos = conn, avr = avrConn, catalogue, state}
    race (tracker chan state conn) (act dev) >>= either absurd pure

tracker :: TChan HeosEvent -> TVar LiveState -> HeosConnection -> IO Void
tracker chan state conn =
  forever $
    atomically (readTChan chan) >>= \case
      HeosEvent "player_volume_changed" p -> forM_ (lookupParam "level" p >>= readMaybe . T.unpack) $ \volume ->
        atomically $ modifyTVar' state $ \s -> s{volume}
      HeosEvent "player_state_changed" p -> forM_ (lookupParam "state" p >>= parsePlayState) $ \playState -> atomically $ modifyTVar' state $ \s -> s{playState}
      HeosEvent "player_now_playing_changed" _ -> handle skipBadReply $ do
        track <- sendCommand conn (GetNowPlayingMedia conn.pid) >>= decodePayload
        atomically $ modifyTVar' state $ \s -> s{nowPlaying = Just track}
      _ -> pure ()
 where
  skipBadReply (ProtocolError msg) = putStrLn ("now playing: " <> T.unpack msg)

-- | The start-up sequence the vendor prescribes: events off, the state
--   queries, events on. Subscription commands go over the events socket,
--   queries over the commands socket.
initialize :: HeosConnection -> IO (Catalogue, LiveState)
initialize conn = do
  let (ConnectionSockets _ eSock cSock) = conn.sockets
      query = sendOn cSock conn.props
      subscribeCmd = sendOn eSock conn.props
  _ <- subscribeCmd (RegisterForChangeEvents Off)
  -- TODO: поставить сюда signIn
  -- То что в сети есть хотя бы один плеер, было проверено в `readPid` в `Heos.Connection.Heos`
  players <- query GetPlayers >>= decodePayload
  sources <- query GetMusicSources >>= decodePayload
  criteria <-
    Map.fromList
      <$> forM
        sources
        ( \ms -> do
            crit <- query (GetSearchCriteria ms.sid) >>= decodePayload
            pure (ms.sid, crit)
        )
  let catalogue = Catalogue{sources, players, criteria}
      pid = conn.pid

  nowPlaying <- query (GetNowPlayingMedia pid) >>= decodePayload
  volume <- query (GetVolume pid) >>= fromMessage "level" (readMaybe . T.unpack)
  playState <- query (GetPlayState pid) >>= fromMessage "state" parsePlayState

  let liveState = LiveState{nowPlaying, volume, playState}
  _ <- subscribeCmd (RegisterForChangeEvents On)

  pure (catalogue, liveState)