module Heos.Device (
  Device (..),
  Catalogue (..),
  LiveState (..),
  withDevice,
  searchable,
  reconnecting,
  initialize,
  tracker,
  withSupervised,
) where

import Control.Arrow ((>>>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (TChan, TVar, atomically, modifyTVar', newTVarIO, readTChan, readTVar, retry, writeTVar)
import Control.Exception (Exception (displayException), finally, handle, throwIO, try)
import Control.Monad (forM, forM_, forever)
import Control.Monad.Catch (Handler (..))
import Control.Retry (recovering)
import Data.Bool (bool)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (find)
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
import Heos.Connection.Types (ConnectionProps (..), ConnectionSockets (..), HeosConnectionParams (..))
import Heos.Device.Parameters (DeviceParams (..))
import Heos.Discovery (discover)
import Heos.Envelope.Heos (HeosEvent (HeosEvent))
import Heos.Error (CommandError (..), ConnectionError (NoPlayers, Unavailable), DiscoveryError)
import Heos.Logger (Logger (..), Severity (..), logAt)
import Heos.Time (toMicros)
import Heos.Types
import Heos.Url (lookupParam)
import Network.Socket (HostAddress, PortNumber, SockAddr (SockAddrInet))
import System.Timeout (timeout)
import Text.Read (readMaybe)

data Device = Device
  { heos :: HeosConnection
  , avr :: Maybe AvrConnection
  , catalogue :: Catalogue
  , state :: TVar LiveState
  }

-- | What the device is: read once at start-up, never changes while the
--   connection lives.
data Catalogue = Catalogue
  { player :: Player
  , players :: [Player]
  , sources :: [MusicSource]
  , criteria :: Map SourceId [SearchCriterion]
  , account :: Maybe Account
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

data Link = Down | Up Device

withSupervised :: DeviceParams -> (IO Device -> IO a) -> IO a
withSupervised params server = do
  link <- newTVarIO Down
  -- prepares supervisor action
  let supervise = withDevice params $ \dev -> (atomically (writeTVar link (Up dev)) >> forever (threadDelay maxBound)) `finally` atomically (writeTVar link Down)
      -- tries getting device from the box, and repeats if it's DOWN
      getDev =
        atomically $
          readTVar link >>= \case
            Up dev -> pure dev
            Down -> retry
      -- action limiting the device search time
      getDevTimeLimited = timeout ms getDev >>= maybe (throwIO Unavailable) pure
  -- concurrently runs the supervisor and the server
  race supervise (server getDevTimeLimited) >>= either absurd pure
 where
  ms = toMicros params.heos.commandTimeout

withDevice :: DeviceParams -> (Device -> IO a) -> IO a
withDevice params = reconnecting params findAddr
 where
  findAddr = do
    hosts <- discover
    let host = NE.head hosts
    logHosts hosts host
    pure host
  logHosts hosts host = do
    let logAt = params.heos.logger.logAt
        numDevs = NE.length hosts
        port = params.heos.port
        addr = showAddr port host
    case numDevs of
      1 -> logAt Debug $ "Found HEOS device: " <> addr
      n -> logAt Warn $ "Found " <> show n <> " HEOS devices, taking " <> show addr

type ShouldRetry e = (Exception e) => e -> IO Bool

reconnecting :: DeviceParams -> IO HostAddress -> (Device -> IO a) -> IO a
reconnecting params findAddr act = go
 where
  logAt = params.heos.logger.logAt
  reconErrLog e = logAt Warn ("Link dropped: " <> displayException e)
  go = do
    established <- newIORef False
    let onRecover _ = maybe findAddr pure params.heos.address >>= withConnection params.heos (deviceInit established)
    res <- try @ConnectionError $ recovering params.heos.retryPolicy (handlers established) onRecover
    either (reconErrLog >>> (>> go)) pure res

  handlers ioRef =
    let logReconnect :: ShouldRetry e
        logReconnect e = True <$ logAt Warn ("Reconnect: " <> displayException e)
        checkAndLogError :: ShouldRetry e
        checkAndLogError e = readIORef ioRef >>= bool (logReconnect e) (pure False)
     in [ const . Handler $ logReconnect @DiscoveryError
        , const . Handler $ checkAndLogError @ConnectionError
        , const . Handler $ checkAndLogError @CommandError
        ]

  deviceInit established conn = withAvr params.avr hostAddr $ \avrConn -> do
    chan <- subscribe conn
    (catalogue, liveState) <- initialize conn
    state <- newTVarIO liveState
    let dev = Device{heos = conn, avr = avrConn, catalogue, state}
    initLog dev
    writeIORef established True
    let concurrent = race (tracker chan state conn) (act dev) >>= either absurd pure
    concurrent `finally` exitLog catalogue
   where
    hostAddr = getAddr conn
    exitLog (Catalogue{player}) = logAt Info ("Disconnected from " <> showAddr conn.props.params.port hostAddr <> ": " <> T.unpack player.model)
    initLog dev = do
      let port = conn.props.params.port
          address = showAddr port hostAddr
          player = dev.catalogue.player
          playerInfo = T.unpack player.model <> ", firmware " <> T.unpack player.version
          numSources = length dev.catalogue.sources
          sourcesInfo = show numSources <> " source" <> (if numSources > 1 then "s" else "")
      logAt Info $ "Connected to " <> address <> ": " <> playerInfo <> ", " <> sourcesInfo

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
  logAt = conn.props.params.logger.logAt
  skipBadReply :: CommandError -> IO ()
  skipBadReply e = logAt Warn ("Now playing: " <> displayException e)

initialize :: HeosConnection -> IO (Catalogue, LiveState)
initialize conn = do
  _ <- subscribeCmd (RegisterForChangeEvents Off)
  catalogue <- catalogueOf conn
  liveState <- snapshot conn
  _ <- subscribeCmd (RegisterForChangeEvents On)
  pure (catalogue, liveState)
 where
  subscribeCmd = sendOn conn.sockets.events conn.props

catalogueOf :: HeosConnection -> IO Catalogue
catalogueOf conn = do
  account <- signIn conn
  players <- sendCommand conn GetPlayers
  player <- maybe (throwIO NoPlayers) pure $ find ((== conn.pid) . (.pid)) players
  sources <- sendCommand conn GetMusicSources
  criteria <- Map.fromList <$> forM sources (\ms -> (ms.sid,) <$> sendCommand conn (GetSearchCriteria ms.sid))
  pure Catalogue{sources, players, criteria, account, player}

snapshot :: HeosConnection -> IO LiveState
snapshot conn = LiveState <$> sendCommand conn (GetNowPlayingMedia conn.pid) <*> sendCommand conn (GetVolume conn.pid) <*> sendCommand conn (GetPlayState conn.pid)

signIn :: HeosConnection -> IO (Maybe Account)
signIn conn = case account of
  Nothing -> pure Nothing
  Just creds ->
    try (sendCommand conn (AccountSignIn creds)) >>= \case
      Right name -> Account <$> name <$ logAt Info ("Signed in as " <> maybe "?" T.unpack name)
      Left (Refused eid msg) -> Nothing <$ logAt Warn ("Sign-in refused: " <> show (eid, msg) <> ", streaming is off")
      Left other -> throwIO other
 where
  logAt = conn.props.params.logger.logAt
  account = conn.props.params.account

showAddr :: PortNumber -> HostAddress -> String
showAddr port hostAddr = show (SockAddrInet port hostAddr)