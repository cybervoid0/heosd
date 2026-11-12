module Heos.Device where

import Control.Concurrent.Async (race)
import Control.Concurrent.STM (TChan, TVar, atomically, modifyTVar', newTVarIO, readTChan)
import Control.Exception (Exception (displayException), handle, throwIO, try)
import Control.Monad (forM, forM_, forever)
import Control.Monad.Catch (Handler (..))
import Control.Retry (recovering)
import Data.Bool (bool)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text qualified as T
import Data.Void (Void, absurd)
import Heos.Command.Heos (Command (..))
import Heos.Connection (HeosConnection (..), getAddr, sendCommand, subscribe, withAvr)
import Heos.Connection.Avr (AvrConnection)
import Heos.Connection.Heos (sendOn, withConnection)
import Heos.Connection.Types (ConnectionSockets (ConnectionSockets), HeosConnectionParams (..))
import Heos.Device.Parameters (DeviceParams (..))
import Heos.Discovery (discover)
import Heos.Envelope.Heos (HeosEvent (HeosEvent))
import Heos.Error (CommandError (..), ConnectionError, DiscoveryError)
import Heos.Types
import Heos.Url (lookupParam)
import Network.Socket (HostAddress)
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
withDevice params = reconnecting params findAddr
 where
  findAddr = discover >>= either throwIO (pure . NE.head)

reconnecting :: DeviceParams -> IO HostAddress -> (Device -> IO a) -> IO a
reconnecting params findAddr act = go
 where
  go = do
    established <- newIORef False
    res <- try @ConnectionError $ recovering params.heos.retryPolicy (handlers established) $ \_ -> findAddr >>= withConnection params.heos (deviceInit established)
    case res of
      Right a -> pure a
      Left e -> putStrLn ("link dropped: " <> displayException e) >> go
  handlers ioRef =
    [ const $ Handler $ \(e :: DiscoveryError) -> True <$ putStrLn ("reconnect: " <> displayException e)
    , const $ Handler $ \(e :: ConnectionError) ->
        readIORef ioRef
          -- если соединение уже установлено, то не переподключаться по политикам, а пробросить исключение наружу, где go начнёт новый виток с чистым счётчиком
          >>= bool (True <$ putStrLn ("reconnect: " <> displayException e)) (pure False)
    ]
  deviceInit established conn = withAvr params.avr (getAddr conn) $ \avrConn -> do
    chan <- subscribe conn
    (catalogue, liveState) <- initialize conn
    state <- newTVarIO liveState
    let dev = Device{heos = conn, avr = avrConn, catalogue, state}
    writeIORef established True
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
  skipBadReply :: CommandError -> IO ()
  skipBadReply e = putStrLn ("now playing: " <> displayException e)

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