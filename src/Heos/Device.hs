module Heos.Device where

import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad (forM)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Heos.Command.Heos (Command (..))
import Heos.Connection (HeosConnection (..), getAddr, withAvr, withReconnect)
import Heos.Connection.Avr (AvrConnection)
import Heos.Connection.Heos (sendOn)
import Heos.Connection.Types (ConnectionSockets (..))
import Heos.Device.Parameters
import Heos.Envelope.Heos (decodePayload, fromMessage)
import Heos.Types
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
    (catalogue, liveState) <- initialize conn
    state <- newTVarIO liveState
    act
      Device
        { heos = conn
        , avr = avrConn
        , catalogue
        , state
        }

-- | The start-up sequence the vendor prescribes: events off, the state
--   queries, events on. Subscription commands go over the events socket,
--   queries over the commands socket. A command without a reply throws,
--   so a half-initialized connection never reaches the action.
initialize :: HeosConnection -> IO (Catalogue, LiveState)
initialize conn = do
  let (ConnectionSockets _ eSock cSock) = conn.sockets
      query = sendOn cSock conn.props
      subscribeCmd = sendOn eSock conn.props
  _ <- subscribeCmd (RegisterForChangeEvents Off)
  -- TODO: поставить сюда signIn
  -- То что в сети есть хотя бы один плеер, было проверено в `readPid` в `Heos.Connection.Heos`
  players <- query GetPlayers >>= decodePayload @[Player]
  sources <- query GetMusicSources >>= decodePayload @[MusicSource]
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

  nowPlaying <- query (GetNowPlayingMedia pid) >>= decodePayload @(Maybe Track)
  volume <- query (GetVolume pid) >>= fromMessage "level" (readMaybe @Int . T.unpack)
  playState <- query (GetPlayState pid) >>= fromMessage "state" parsePlayState

  let liveState = LiveState{nowPlaying, volume, playState}
  _ <- subscribeCmd (RegisterForChangeEvents On)

  pure (catalogue, liveState)