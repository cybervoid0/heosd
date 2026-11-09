-- | The Live handler over a fake device: each action of the effect must
--   turn into the right command on the wire, each query must be answered
--   from the cache without a command, and each reply must become the right
--   value. Nothing here is about sockets; that is ConnectionSpec's job.
module Heos.LiveSpec (spec) where

import Control.Concurrent (newEmptyMVar)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (bracket)
import Control.Retry (constantDelay)
import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful (Eff, IOE, runEff)
import Heos.Connection.Avr (AvrConnection (..), AvrConnectionParams (..))
import Heos.Connection.Heos (runConnection)
import Heos.Connection.Types (ConnectionSockets (..), HeosConnection (..), HeosConnectionParams (..))
import Heos.Device (Catalogue (..), Device (..), LiveState (..))
import Heos.Effect.Heos
import Heos.Envelope.Heos (HeosError (..), ProtocolError, decodePayload)
import Heos.FakeDevice (answering, answeringWith, commandPath, commandQuery, failReply, fixture, replyTo, scripted)
import Heos.Handler.Live (runHeosLive)
import Heos.Time (Seconds (..))
import Heos.Types
import Network.Socket (Family (AF_UNIX), SocketType (Stream), close, socketPair, tupleToHostAddress)
import System.FilePath ((</>))
import Test.Hspec

heosParams :: HeosConnectionParams
heosParams = HeosConnectionParams{commandTimeout = Seconds 1, heartbeatDelay = Seconds 3600, maxLineLength = 64 * 1024, port = 1255, retryPolicy = constantDelay 10000}

avrParams :: AvrConnectionParams
avrParams = AvrConnectionParams{port = 23, requestTimeout = Seconds 1}

-- The payload of a recorded reply, decoded.
recorded :: (FromJSON a) => FilePath -> IO a
recorded file =
  eitherDecodeFileStrict @Envelope ("test/fixtures/responses" </> file)
    >>= either fail pure
    >>= decodePayload

-- What the device would have been found with: the recorded players and
-- sources, Tidal's criteria for Tidal (sid 10), none for TuneIn (sid 3).
catalogueOf :: IO Catalogue
catalogueOf = do
  players <- recorded "get_players.json"
  sources <- recorded "get_music_sources.json"
  tidal <- recorded "search_criteria.json"
  pure Catalogue{players, sources, criteria = Map.fromList [(SourceId 10, tidal), (SourceId 3, [])]}

-- The cache as the tracker left it.
cached :: LiveState
cached = LiveState{nowPlaying = Nothing, volume = 15, playState = Play}

-- Run a program against the Live handler over a device whose HEOS
-- connection goes to a fake answering with `reply`. The AVR side is never
-- touched by this handler, so it is an empty cell. Returns the result and
-- the commands the device saw on the commands socket, in order.
withLive :: (ByteString -> ByteString) -> Eff '[Heos, IOE] a -> IO (a, [ByteString])
withLive reply prog =
  withPair $ \(ourEvents, devEvents) ->
    withPair $ \(ourCommands, devCommands) -> do
      seen <- newTVarIO []
      catalogue <- catalogueOf
      state <- newTVarIO cached
      avr <- (`AvrConnection` avrParams) <$> newEmptyMVar
      let record line = atomically (modifyTVar' seen (line :))
      withAsync (answering (const (pure ())) devEvents) $ \_ ->
        withAsync (answeringWith reply record devCommands) $ \_ -> do
          -- the player is not asked for here: pid 1 is set by hand
          let sockets = ConnectionSockets (tupleToHostAddress (127, 0, 0, 1)) ourEvents ourCommands
          result <- runConnection heosParams sockets $ \props -> do
            let heos = HeosConnection{pid = PlayerId 1, sockets, props}
            runEff (runHeosLive Device{heos, avr, catalogue, state} prog)
          commands <- reverse <$> readTVarIO seen
          pure (result, commands)
 where
  withPair = bracket (socketPair AF_UNIX Stream 0) (\(a, b) -> close a >> close b)

-- The commands the device saw, as (path, query without SEQUENCE).
sent :: [ByteString] -> [(ByteString, ByteString)]
sent = map (\l -> (commandPath l, dropSeq (commandQuery l)))
 where
  dropSeq q = BS.intercalate "&" [p | p <- BC.split '&' q, not ("SEQUENCE=" `BS.isPrefixOf` p)]

isProtocolError :: ProtocolError -> Bool
isProtocolError _ = True

spec :: Spec
spec = describe "runHeosLife" $ do
  describe "commands going out" $ do
    it "setVolume becomes player/set_volume for the configured player" $ do
      (_, cmds) <- withLive replyTo (setVolume 7)
      sent cmds `shouldBe` [("player/set_volume", "pid=1&level=7")]

    it "volumeUp and volumeDown carry the step" $ do
      (_, cmds) <- withLive replyTo (volumeUp 2 >> volumeDown 3)
      sent cmds `shouldBe` [("player/volume_up", "pid=1&step=2"), ("player/volume_down", "pid=1&step=3")]

    it "play, pause and stop render the state" $ do
      (_, cmds) <- withLive replyTo (play >> pause >> stop)
      sent cmds `shouldBe` [("player/set_play_state", "pid=1&state=play"), ("player/set_play_state", "pid=1&state=pause"), ("player/set_play_state", "pid=1&state=stop")]

    -- Mute is a switch of its own on the device, not volume 0: the level
    -- survives it, and the device toggles it in one command.
    it "mute becomes player/toggle_mute, and does not set the volume" $ do
      (_, cmds) <- withLive replyTo mute
      sent cmds `shouldBe` [("player/toggle_mute", "pid=1")]

    it "clearQueue becomes player/clear_queue for the configured player" $ do
      (_, cmds) <- withLive replyTo clearQueue
      sent cmds `shouldBe` [("player/clear_queue", "pid=1")]

    -- Easy to swap, and nothing but a test would notice.
    it "playNext and playPrevious are not mixed up" $ do
      (_, cmds) <- withLive replyTo (next >> prev)
      map fst (sent cmds) `shouldBe` ["player/play_next", "player/play_previous"]

    it "enqueue of a track sends sid, cid, mid and the mode" $ do
      (_, cmds) <- withLive replyTo (enqueue AddToEnd (PlayTrack (SourceId 10) (ContainerId "SEARCHED_TRACKS-x") (MediaId "5")))
      sent cmds `shouldBe` [("browse/add_to_queue", "pid=1&sid=10&cid=SEARCHED_TRACKS-x&mid=5&aid=3")]

    it "enqueue of a container sends sid and cid, no mid" $ do
      (_, cmds) <- withLive replyTo (enqueue AddPlayNow (PlayContainer (SourceId 10) (ContainerId "77")))
      sent cmds `shouldBe` [("browse/add_to_queue", "pid=1&sid=10&cid=77&aid=1")]

    -- What the device answered on a bare album_id as cid: the refusal must
    -- reach the caller as the device's own eid and text, not as a quiet ().
    it "a refused playItem throws the device's error" $ do
      let refused = scripted [("browse/add_to_queue", failReply "browse/add_to_queue" "eid=14&text=cannot play")]
      withLive refused (playItem (PlayContainer (SourceId 10) (ContainerId "443242286")))
        `shouldThrow` (== HeosError{eid = Just 14, text = Just "cannot play"})

    -- The first replaces the queue and starts, the rest line up behind it.
    it "playAll replaces the queue with the first item and appends the rest" $ do
      let item n = PlayContainer (SourceId 10) (ContainerId n)
      (_, cmds) <- withLive replyTo (playAll [item "a", item "b", item "c"])
      map snd (sent cmds) `shouldBe` ["pid=1&sid=10&cid=a&aid=4", "pid=1&sid=10&cid=b&aid=3", "pid=1&sid=10&cid=c&aid=3"]

  -- The queue is not cached: each look asks the device, and the whole
  -- queue comes back (no range), decoded from what the device sends.
  describe "queue" $ do
    it "showQueue asks the device for the whole queue and decodes it" $ do
      queue <- fixture "get_queue.json"
      (items, cmds) <- withLive (scripted [("player/get_queue", queue)]) showQueue
      sent cmds `shouldBe` [("player/get_queue", "pid=1")]
      length items `shouldBe` 75
      map (.song) (take 2 items) `shouldBe` ["Heaven's Gonna Burn Your Eyes", "Facing East"]
      map (.qid) (take 2 items) `shouldBe` [QueueId 1, QueueId 2]

    it "a queue reply without payload is a ProtocolError" $
      withLive replyTo showQueue `shouldThrow` isProtocolError

  -- Stage 30's check: "what plays" is answered at once, from the cache the
  -- tracker keeps, and the device is not asked.
  describe "queries answered from the cache" $ do
    it "getVolume comes from the cache, no command sent" $ do
      (v, cmds) <- withLive replyTo getVolume
      v `shouldBe` 15
      cmds `shouldBe` []

    it "getPlayState comes from the cache, no command sent" $ do
      (s, cmds) <- withLive replyTo getPlayState
      s `shouldBe` Play
      cmds `shouldBe` []

    it "getNowPlaying comes from the cache, no command sent" $ do
      (t, cmds) <- withLive replyTo getNowPlaying
      t `shouldBe` Nothing
      cmds `shouldBe` []

    it "getPlayers and getSources come from the catalogue, no command sent" $ do
      ((ps, ss), cmds) <- withLive replyTo ((,) <$> getPlayers <*> getSources)
      map (.name) ps `shouldBe` ["Marantz MODEL 60n"]
      length ss `shouldBe` 11
      cmds `shouldBe` []

  describe "replies coming back" $ do
    -- A reply the handler cannot make sense of is a ProtocolError, not a
    -- silent default: here the device answered the search without results.
    it "a reply without payload where one is expected is a ProtocolError" $
      withLive replyTo (search (SourceId 10) TrackKind "x") `shouldThrow` isProtocolError

  describe "search" $ do
    -- Which sources can take the search is known since start-up too: from
    -- the recorded catalogue only Tidal is available, signed in and lists
    -- the criteria; TuneIn listed none.
    it "getSearchable answers from the catalogue, without asking the device" $ do
      (sids, cmds) <- withLive replyTo ((,) <$> getSearchable TrackKind <*> getSearchable PlaylistKind)
      sids `shouldBe` ([SourceId 10], [SourceId 10])
      cmds `shouldBe` []

    -- The criteria are in the catalogue since start-up: the search goes
    -- straight to the device with the matching scid. A found song is played
    -- out of its album container (LIBALBUM-<album_id> on Tidal): the
    -- SEARCHED_TRACKS- container the spec describes the MODEL 60n rejects.
    it "searches with the scid from the catalogue and returns playable tracks" $ do
      results <- fixture "search_ok_computer.json"
      (found, cmds) <-
        withLive
          (scripted [("browse/search", results)])
          (search (SourceId 10) TrackKind "Ok Computer")
      sent cmds `shouldBe` [("browse/search", "sid=10&search=Ok Computer&scid=3")]
      length found `shouldBe` 50
      map (.target) found
        `shouldSatisfy` all (\case PlayTrack (SourceId 10) (ContainerId cid) _ -> "LIBALBUM-" `T.isPrefixOf` cid; _ -> False)

    it "picks the scid the device gave for the kind, not a fixed number" $ do
      results <- fixture "search_ok_computer.json"
      (_, cmds) <-
        withLive
          (scripted [("browse/search", results)])
          (search (SourceId 10) PlaylistKind "chill")
      map snd (sent cmds) `shouldBe` ["sid=10&search=chill&scid=6"]

    it "fails with a ProtocolError, without searching, when the source has no such criterion" $ do
      withLive replyTo (search (SourceId 3) TrackKind "x") `shouldThrow` isProtocolError

    it "fails with a ProtocolError, without searching, for a source the device does not list" $ do
      withLive replyTo (search (SourceId 99) TrackKind "x") `shouldThrow` isProtocolError
