-- | 'Heos.Device' over a fake device: the start-up sequence (what the daemon
--   asks once a connection is up, in what order, what it keeps of the
--   answers) and the tracker that keeps the live state current from the
--   events. The device is a pair of socket pairs answering from recorded
--   replies; nothing here is about sockets as such, that is ConnectionSpec.
module Heos.DeviceSpec (spec) where

import Control.Concurrent.Async (race, withAsync)
import Control.Concurrent.STM (TVar, atomically, check, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Exception (bracket)
import Data.Void (absurd)
import Control.Retry (constantDelay)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Map.Strict qualified as Map
import Heos.Connection.Heos (runConnection, subscribe)
import Heos.Connection.Types (ConnectionSockets (..), HeosConnection (..), HeosConnectionParams (..))
import Data.Text (Text)
import Heos.Device (Catalogue (..), LiveState (..), initialize, searchable, tracker)
import Heos.Envelope.Heos (ProtocolError (..))
import Heos.FakeDevice (answeringWith, commandPath, commandQuery, failReply, fixture, messageReply, scripted)
import Heos.Time (Seconds (..))
import Heos.Types
import Network.Socket (Family (AF_UNIX), Socket, SocketType (Stream), close, socketPair, tupleToHostAddress)
import Network.Socket.ByteString (sendAll)
import System.Timeout (timeout)
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

-- A connection to a device answering with `reply` on both lanes. The test
-- gets the connection and the device's end of the events lane, to push
-- events through. Returns the action's result and the commands the device
-- saw, with their lanes, in order.
withFake :: (ByteString -> ByteString) -> (HeosConnection -> Socket -> IO a) -> IO (a, [(Lane, ByteString)])
withFake = withFakeWith params

-- 'withFake' with the connection parameters chosen by the test, for the
-- tests that need a short command timeout.
withFakeWith :: HeosConnectionParams -> (ByteString -> ByteString) -> (HeosConnection -> Socket -> IO a) -> IO (a, [(Lane, ByteString)])
withFakeWith params' reply act =
  withPair $ \(ourEvents, devEvents) ->
    withPair $ \(ourCommands, devCommands) -> do
      seen <- newTVarIO []
      withAsync (answer seen EventsLane devEvents) $ \_ ->
        withAsync (answer seen CommandsLane devCommands) $ \_ -> do
          let sockets = ConnectionSockets (tupleToHostAddress (127, 0, 0, 1)) ourEvents ourCommands
          result <- runConnection params' sockets $ \props ->
            act HeosConnection{pid = thePid, sockets, props} devEvents
          commands <- reverse <$> readTVarIO seen
          pure (result, commands)
 where
  withPair = bracket (socketPair AF_UNIX Stream 0) (\(a, b) -> close a >> close b)
  answer :: TVar [(Lane, ByteString)] -> Lane -> Socket -> IO ()
  answer seen lane = answeringWith reply (\line -> atomically (modifyTVar' seen ((lane, line) :)))

withStartUp :: (ByteString -> ByteString) -> IO ((Catalogue, LiveState), [(Lane, ByteString)])
withStartUp reply = withFake reply (\conn _ -> initialize conn)

-- The query without its SEQUENCE number.
dropSeq :: ByteString -> ByteString
dropSeq q = BS.intercalate "&" [p | p <- BC.split '&' q, not ("SEQUENCE=" `BS.isPrefixOf` p)]

-- Wire format of an event: one JSON object per line, "\r\n" at the end.
eventLine :: ByteString -> ByteString -> ByteString
eventLine name msg =
  "{\"heos\": {\"command\": \"event/" <> name <> "\", \"message\": \"" <> msg <> "\"}}\r\n"

-- What the live state looks like before any event.
initial :: LiveState
initial = LiveState{nowPlaying = Nothing, volume = 10, playState = Stop}

-- Run the tracker over a connection to a device answering with `reply`,
-- push the events, and hand the state to the check. The tracker runs in
-- the background the way 'withDevice' runs it, under 'race': if it dies,
-- the test fails with its exception at once, instead of waiting out a
-- state that will never come.
withTracker :: (ByteString -> ByteString) -> [ByteString] -> (TVar LiveState -> IO a) -> IO a
withTracker = withTrackerWith params

withTrackerWith :: HeosConnectionParams -> (ByteString -> ByteString) -> [ByteString] -> (TVar LiveState -> IO a) -> IO a
withTrackerWith params' reply events check' =
  fst <$> withFakeWith params' reply (\conn devEvents -> do
    chan <- subscribe conn
    state <- newTVarIO initial
    race (tracker chan state conn) (mapM_ (sendAll devEvents) events >> check' state)
      >>= either absurd pure)

-- Wait for the state to satisfy the predicate; a second is plenty, and a
-- state that never gets there fails the test instead of hanging it.
awaitState :: TVar LiveState -> (LiveState -> Bool) -> IO LiveState
awaitState state p =
  timeout 1000000 (atomically (readTVar state >>= \s -> check (p s) >> pure s))
    >>= maybe (fail "the live state did not change within a second") pure

-- A catalogue made by hand: sources with the three things 'searchable'
-- looks at, and the criteria the device listed for each.
catalogueWith :: [MusicSource] -> [(SourceId, [SearchCriterion])] -> Catalogue
catalogueWith sources criteria = Catalogue{players = [], sources, criteria = Map.fromList criteria}

source :: Text -> Int -> Bool -> Maybe Text -> MusicSource
source name n available serviceUsername =
  MusicSource{name, imageUrl = "", sourceType = "music_service", sid = SourceId n, available, serviceUsername}

criterion :: Text -> Int -> SearchCriterion
criterion name n = SearchCriterion{name, scid = SearchCriteriaId n, wildcard = False, cid = Nothing, playable = Nothing}

trackAndAlbum :: [SearchCriterion]
trackAndAlbum = [criterion "Track" 3, criterion "Album" 2]

spec :: Spec
spec = do
  -- Stage 31's first step: where can this kind of search go at all. Three
  -- sieves, all from what the device said at start-up: the source is
  -- available, someone is signed in, and it lists a criterion of the kind.
  describe "searchable" $ do
    let tidal = source "Tidal" 10 True (Just "listener@example.com")

    it "lists a source that is available, signed in and searchable by the kind" $
      searchable TrackKind (catalogueWith [tidal] [(SourceId 10, trackAndAlbum)]) `shouldBe` [SourceId 10]

    it "leaves out a source that is not available" $
      searchable TrackKind (catalogueWith [source "Deezer" 5 False (Just "x")] [(SourceId 5, trackAndAlbum)]) `shouldBe` []

    it "leaves out a source nobody is signed in to" $
      searchable TrackKind (catalogueWith [source "Qobuz" 30 True Nothing] [(SourceId 30, trackAndAlbum)]) `shouldBe` []

    it "leaves out a source without a criterion of that kind" $
      searchable PlaylistKind (catalogueWith [tidal] [(SourceId 10, trackAndAlbum)]) `shouldBe` []

    it "leaves out a source the device listed no criteria for" $
      searchable TrackKind (catalogueWith [tidal] []) `shouldBe` []

    it "keeps the device's order when several qualify" $ do
      let qobuz = source "Qobuz" 30 True (Just "someone")
          both = [(SourceId 10, trackAndAlbum), (SourceId 30, trackAndAlbum)]
      searchable AlbumKind (catalogueWith [qobuz, tidal] both) `shouldBe` [SourceId 30, SourceId 10]

  describe "initialize" $ do
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

  -- Stage 30's check: "what plays" is answered from the cache, so the cache
  -- has to follow the device. The events carry volume and play state
  -- themselves; a change of track is only announced, and the tracker asks.
  describe "tracker" $ do
    it "takes the volume from player_volume_changed" $ do
      reply <- startUpReplies
      withTracker reply [eventLine "player_volume_changed" "pid=-1245882071&level=42&mute=off"] $ \state -> do
        live <- awaitState state ((== 42) . (.volume))
        live.playState `shouldBe` Stop

    it "takes the play state from player_state_changed" $ do
      reply <- startUpReplies
      withTracker reply [eventLine "player_state_changed" "pid=-1245882071&state=pause"] $ \state -> do
        live <- awaitState state ((== Pause) . (.playState))
        live.volume `shouldBe` 10

    it "asks the device what plays on player_now_playing_changed" $ do
      reply <- startUpReplies
      ((), commands) <- withFake reply $ \conn devEvents -> do
        chan <- subscribe conn
        state <- newTVarIO initial
        withAsync (tracker chan state conn) $ \_ -> do
          sendAll devEvents (eventLine "player_now_playing_changed" "pid=-1245882071")
          live <- awaitState state ((/= Nothing) . (.nowPlaying))
          fmap (.song) live.nowPlaying `shouldBe` Just "Lebanese Blonde"
      map (fmap commandPath) commands `shouldBe` [(CommandsLane, "player/get_now_playing_media")]

    -- An event the tracker cannot read is not a reason to guess: the state
    -- keeps its value, and the tracker goes on with the next event.
    it "leaves the state alone on an event without a readable value, and carries on" $ do
      reply <- startUpReplies
      let events =
            [ eventLine "player_volume_changed" "pid=-1245882071&mute=off"
            , eventLine "player_state_changed" "pid=-1245882071&state=sideways"
            , eventLine "player_volume_changed" "pid=-1245882071&level=7&mute=off"
            ]
      withTracker reply events $ \state -> do
        live <- awaitState state ((== 7) . (.volume))
        live.playState `shouldBe` Stop

    -- A now-playing reply without payload is a bad reply, not a dead
    -- connection: the track stays as it was and the tracker survives.
    it "survives a now-playing reply it cannot decode" $ do
      reply <- startUpReplies
      let broken line
            | commandPath line == "player/get_now_playing_media" = messageReply "player/get_now_playing_media" "pid=-1245882071"
            | otherwise = reply line
          events =
            [ eventLine "player_now_playing_changed" "pid=-1245882071"
            , eventLine "player_volume_changed" "pid=-1245882071&level=3&mute=off"
            ]
      withTracker broken events $ \state -> do
        live <- awaitState state ((== 3) . (.volume))
        live.nowPlaying `shouldBe` Nothing

    -- The device refuses the query (result=fail). That is the device's
    -- answer, not a dead link: the tracker notes it and goes on. Today the
    -- HeosError kills the tracker, the race cancels the user's action, and
    -- since HeosError is not one of the reconnect handlers' exceptions, the
    -- whole daemon dies over one refused command.
    it "survives a now-playing reply the device refuses" $ do
      reply <- startUpReplies
      let refusing line
            | commandPath line == "player/get_now_playing_media" = failReply "player/get_now_playing_media" "eid=12&text=System error"
            | otherwise = reply line
          events =
            [ eventLine "player_now_playing_changed" "pid=-1245882071"
            , eventLine "player_volume_changed" "pid=-1245882071&level=3&mute=off"
            ]
      withTracker refusing events $ \state -> do
        live <- awaitState state ((== 3) . (.volume))
        live.nowPlaying `shouldBe` Nothing

    -- The device never answers the query. Whether the link is alive is the
    -- heartbeat's call, not the tracker's: it gives up on this track and
    -- goes on with the next event. Today the ConnectionTimeout kills the
    -- tracker, and with it the connection.
    it "survives a now-playing query the device never answers" $ do
      reply <- startUpReplies
      let silent line
            | commandPath line == "player/get_now_playing_media" = "" -- nothing goes back
            | otherwise = reply line
          events =
            [ eventLine "player_now_playing_changed" "pid=-1245882071"
            , eventLine "player_volume_changed" "pid=-1245882071&level=3&mute=off"
            ]
      withTrackerWith params{commandTimeout = Seconds 0.1} silent events $ \state -> do
        live <- awaitState state ((== 3) . (.volume))
        live.nowPlaying `shouldBe` Nothing

    it "ignores events about other things" $ do
      reply <- startUpReplies
      let events =
            [ eventLine "player_queue_changed" "pid=-1245882071"
            , eventLine "player_volume_changed" "pid=-1245882071&level=5&mute=off"
            ]
      withTracker reply events $ \state -> do
        live <- awaitState state ((== 5) . (.volume))
        live.playState `shouldBe` Stop
