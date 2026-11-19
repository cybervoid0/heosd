module Heos.Device (
  Device (..),
  Catalogue (..),
  LiveState (..),
  withDevice,
  searchable,
  reconnecting,
  initialize,
  tracker,
) where

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
import Heos.Connection.Types (ConnectionProps (..), ConnectionSockets (ConnectionSockets), HeosConnectionParams (..))
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

withDevice :: DeviceParams -> (Device -> IO a) -> IO a
withDevice params = reconnecting params (NE.head <$> discover)

type ShouldRetry e = (Exception e) => e -> IO Bool
reconnecting :: DeviceParams -> IO HostAddress -> (Device -> IO a) -> IO a
reconnecting params findAddr act = go
 where
  go = do
    established <- newIORef False
    let onRecover _ = findAddr >>= withConnection params.heos (deviceInit established)
    res <- try @ConnectionError $ recovering params.heos.retryPolicy (handlers established) onRecover
    either (\e -> putStrLn ("link dropped: " <> displayException e) >> go) pure res

  handlers ioRef =
    let logReconnect :: ShouldRetry e
        logReconnect e = True <$ putStrLn ("reconnect: " <> displayException e)
        checkAndLogError :: ShouldRetry e
        checkAndLogError e = readIORef ioRef >>= bool (logReconnect e) (pure False)
     in [ const . Handler $ logReconnect @DiscoveryError
        , const . Handler $ checkAndLogError @ConnectionError
        , const . Handler $ checkAndLogError @CommandError
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
type Query = forall a. Command a -> IO a

initialize :: HeosConnection -> IO (Catalogue, LiveState)
initialize conn = do
  _ <- subscribeCmd (RegisterForChangeEvents Off)
  catalogue <- catalogueOf query conn.props.params.account
  liveState <- snapshot query conn.pid
  _ <- subscribeCmd (RegisterForChangeEvents On)
  pure (catalogue, liveState)
 where
  (ConnectionSockets _ eSock cSock) = conn.sockets
  query :: Query
  query = sendOn cSock conn.props
  subscribeCmd = sendOn eSock conn.props

catalogueOf :: Query -> Maybe Credentials -> IO Catalogue
catalogueOf query creds = do
  account <- signIn query creds
  players <- query GetPlayers
  sources <- query GetMusicSources
  criteria <- Map.fromList <$> forM sources (\ms -> (ms.sid,) <$> query (GetSearchCriteria ms.sid))
  pure Catalogue{sources, players, criteria, account}

snapshot :: Query -> PlayerId -> IO LiveState
snapshot query pid = LiveState <$> query (GetNowPlayingMedia pid) <*> query (GetVolume pid) <*> query (GetPlayState pid)

signIn :: Query -> Maybe Credentials -> IO (Maybe Account)
signIn _ Nothing = pure Nothing
signIn query (Just creds) =
  try (query (AccountSignIn creds)) >>= \case
    Right name -> Account <$> name <$ putStrLn ("signed in as " <> maybe "?" T.unpack name)
    Left (Refused eid msg) -> Nothing <$ putStrLn ("sign-in refused: " <> show (eid, msg) <> ", streaming is off")
    Left other -> throwIO other
