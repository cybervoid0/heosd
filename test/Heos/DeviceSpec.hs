-- | The start-up sequence of 'Heos.Device': what the daemon asks the device
--   once a connection is up, in what order, and what it keeps of the
--   answers. The device is a pair of socket pairs answering from recorded
--   replies; nothing here is about sockets as such, that is ConnectionSpec.
module Heos.DeviceSpec (spec) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (bracket)
import Control.Retry (constantDelay)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Map.Strict qualified as Map
import Heos.Connection.Heos (runConnection)
import Heos.Connection.Types (ConnectionSockets (..), HeosConnection (..))
import Heos.Device (initialize)
import Heos.FakeDevice (answeringWith, commandPath, commandQuery, fixture, messageReply, scripted)
import Heos.Time (Seconds (..))
import Heos.Types
import Network.Socket (Family (AF_UNIX), Socket, SocketType (Stream), close, socketPair, tupleToHostAddress)
import Test.Hspec

params :: HeosConnectionParams
params = HeosConnectionParams{commandTimeout = Seconds 1, heartbeatDelay = Seconds 3600, maxLineLength = 64 * 1024, port = 1255, retryPolicy = constantDelay 10000}

-- The player the connection came up with; the fixtures are recorded from it.
thePid :: PlayerId
thePid = PlayerId (-1245882071)

-- Which lane a command arrived on.
data Lane = EventsLane | CommandsLane deriving (Show, Eq)

-- The recorded replies to every start-up query. The same criteria for every
-- source: the fixture is Tidal's, and the test only counts and keys them.
startUpReplies :: IO (ByteString -> ByteString)
startUpReplies = do
  players <- fixture "get_players.json"
  sources <- fixture "get_music_sources.json"
  criteria <- fixture "search_criteria.json"
  nowPlaying <- fixture "now_playing.json"
  pure $
    scripted
      [ ("player/get_players", players)
      , ("browse/get_music_sources", sources)
      , ("browse/get_search_criteria", criteria)
      , ("player/get_now_playing_media", nowPlaying)
      , ("player/get_volume", messageReply "player/get_volume" "pid=-1245882071&level=15")
      , ("player/get_play_state", messageReply "player/get_play_state" "pid=-1245882071&state=play")
      ]

-- Run initialize over a connection to a device answering with `reply` on
-- both lanes. Returns what initialize returns and the commands the device
-- saw, with their lanes, in order.
withStartUp :: (ByteString -> ByteString) -> IO ((Catalogue, LiveState), [(Lane, ByteString)])
withStartUp reply =
  withPair $ \(ourEvents, devEvents) ->
    withPair $ \(ourCommands, devCommands) -> do
      seen <- newTVarIO []
      withAsync (answer seen EventsLane devEvents) $ \_ ->
        withAsync (answer seen CommandsLane devCommands) $ \_ -> do
          let sockets = ConnectionSockets (tupleToHostAddress (127, 0, 0, 1)) ourEvents ourCommands
          result <- runConnection params sockets $ \props ->
            initialize HeosConnection{pid = thePid, sockets, props}
          commands <- reverse <$> readTVarIO seen
          pure (result, commands)
 where
  withPair = bracket (socketPair AF_UNIX Stream 0) (\(a, b) -> close a >> close b)
  answer :: TVar [(Lane, ByteString)] -> Lane -> Socket -> IO ()
  answer seen lane = answeringWith reply (\line -> atomically (modifyTVar' seen ((lane, line) :)))

-- The query without its SEQUENCE number.
dropSeq :: ByteString -> ByteString
dropSeq q = BS.intercalate "&" [p | p <- BC.split '&' q, not ("SEQUENCE=" `BS.isPrefixOf` p)]

spec :: Spec
spec = describe "initialize" $ do
  -- The order the vendor prescribes: events off first, so the state
  -- queries aren't interleaved with notifications; events on last.
  -- Subscriptions go over the events socket, queries over the commands
  -- socket, and the events socket never carries a query.
  it "runs the start-up sequence in the prescribed order, each command on its lane" $ do
    reply <- startUpReplies
    (_, commands) <- withStartUp reply
    map (fmap commandPath) commands
      `shouldBe` [(EventsLane, "system/register_for_change_events"), (CommandsLane, "player/get_players"), (CommandsLane, "browse/get_music_sources")]
        <> replicate 11 (CommandsLane, "browse/get_search_criteria")
        <> [ (CommandsLane, "player/get_now_playing_media")
           , (CommandsLane, "player/get_volume")
           , (CommandsLane, "player/get_play_state")
           , (EventsLane, "system/register_for_change_events")
           ]
    let enables = [BC.takeWhile (/= '&') q | q <- map (commandQuery . snd) commands, "enable=" `BS.isPrefixOf` q]
    enables `shouldBe` ["enable=off", "enable=on"]

  -- The player queries go to the connection's player, not to a fixed one.
  it "asks about the connection's player" $ do
    reply <- startUpReplies
    (_, commands) <- withStartUp reply
    filter (BS.isPrefixOf "pid=-1245882071" . commandQuery . snd) commands `shouldSatisfy` ((== 3) . length)

  -- Source numbers are never hard-coded: the criteria are asked for every
  -- source the device lists, and kept under that source's sid.
  it "asks the criteria of every source and keys them by sid" $ do
    reply <- startUpReplies
    ((catalogue, _), commands) <- withStartUp reply
    let sids = map (.sid) catalogue.sources
    length sids `shouldBe` 11
    Map.keys catalogue.criteria `shouldBe` Map.keys (Map.fromList [(s, ()) | s <- sids])
    [dropSeq (commandQuery c) | (_, c) <- commands, commandPath c == "browse/get_search_criteria"]
      `shouldBe` ["sid=" <> BC.pack (show s) | SourceId s <- sids]
    map length (Map.elems catalogue.criteria) `shouldSatisfy` all (== 4)

  it "keeps the players the device lists" $ do
    reply <- startUpReplies
    ((catalogue, _), _) <- withStartUp reply
    map (.pid) catalogue.players `shouldBe` [thePid]

  -- The live state starts from the replies; from here on the events keep
  -- it current.
  it "takes what plays, the volume and the play state from the replies" $ do
    reply <- startUpReplies
    ((_, live), _) <- withStartUp reply
    live.volume `shouldBe` 15
    live.playState `shouldBe` Play
    fmap (.song) live.nowPlaying `shouldBe` Just "Lebanese Blonde"

  -- A start-up reply the daemon cannot make sense of is a ProtocolError:
  -- half a state is no state.
  it "fails with a ProtocolError when a reply has no payload" $ do
    reply <- startUpReplies
    let broken line
          | commandPath line == "browse/get_music_sources" = messageReply "browse/get_music_sources" ""
          | otherwise = reply line
    withStartUp broken `shouldThrow` (\(ProtocolError _) -> True)
