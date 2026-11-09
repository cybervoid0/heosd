module Heos.Device where

import Control.Concurrent.Async (race)
import Control.Concurrent.STM (TChan, TVar, atomically, modifyTVar', newTVarIO, readTChan)
import Control.Exception (handle)
import Control.Monad (forM, forM_, forever)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text qualified as T
import Data.Void (Void, absurd)
import Heos.Command.Heos (Command (..))
import Heos.Connection (HeosConnection (..), getAddr, sendCommand, subscribe, withAvr, withReconnect)
import Heos.Connection.Avr (AvrConnection)
import Heos.Connection.Heos (sendOn)
import Heos.Connection.Types (ConnectionSockets (..))
import Heos.Device.Parameters
import Heos.Envelope.Heos (HeosEvent (HeosEvent), ProtocolError (..))
import Heos.Types
import Heos.Url (lookupParam)
import Text.Read (readMaybe)

data Device = Device
  { heos :: HeosConnection
  , avr :: AvrConnection
  , catalogue :: Catalogue
  , state :: TVar LiveState
  }

-- | What the device is: read once at start-up, never changes while the
--   connection lives.
data Catalogue = Catalogue
  { players :: [Player]
  , sources :: [MusicSource]
  , criteria :: Map SourceId [SearchCriterion]
  }

searchable :: SearchKind -> Catalogue -> [SourceId]
searchable kind cat =
  [ s.sid
  | s <- cat.sources
  , s.available
  , isJust s.serviceUsername
  , isJust (Map.lookup s.sid cat.criteria >>= criterionFor kind)
  ]

-- | What the device is doing: read at start-up, kept current by 'tracker'.
data LiveState = LiveState
  { nowPlaying :: Maybe Track
  , volume :: Int
  , playState :: PlayState
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
        nowPlaying <- sendCommand conn (GetNowPlayingMedia conn.pid)
        atomically $ modifyTVar' state $ \s -> s{nowPlaying}
      _ -> pure ()
 where
  skipBadReply (ProtocolError msg) = putStrLn ("now playing: " <> T.unpack msg)

-- | The start-up sequence the vendor prescribes: events off, the state
--   queries, events on. Subscription commands go over the events socket,
--   queries over the commands socket.
initialize :: HeosConnection -> IO (Catalogue, LiveState)
initialize conn = do
  let (ConnectionSockets _ eSock cSock) = conn.sockets
      query :: Command a -> IO a
      query = sendOn cSock conn.props
      subscribeCmd = sendOn eSock conn.props
  _ <- subscribeCmd (RegisterForChangeEvents Off)
  -- TODO: поставить сюда signIn
  -- То что в сети есть хотя бы один плеер, было проверено в `readPid` в `Heos.Connection.Heos`
  players <- query GetPlayers
  sources <- query GetMusicSources
  criteria <-
    Map.fromList
      <$> forM
        sources
        ( \ms -> do
            crit <- query (GetSearchCriteria ms.sid)
            pure (ms.sid, crit)
        )
  let catalogue = Catalogue{sources, players, criteria}
      pid = conn.pid

  nowPlaying <- query (GetNowPlayingMedia pid)
  volume <- query (GetVolume pid)
  playState <- query (GetPlayState pid)

  let liveState = LiveState{nowPlaying, volume, playState}
  _ <- subscribeCmd (RegisterForChangeEvents On)

  pure (catalogue, liveState)