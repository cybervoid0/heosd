-- | The Live handler over a fake device: each operation of the effect must
--   turn into the right command on the wire, and each reply into the right
--   value. Nothing here is about sockets; that is ConnectionSpec's job.
module Heos.LiveSpec (spec) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Maybe (fromMaybe)
import Effectful (Eff, IOE, runEff)
import Heos.Command (QueueMode (..))
import Heos.Connection.Internal (ConnectionSockets (..), runConnection)
import Heos.Connection.Parameters (ConnectionParams (..))
import Heos.Effect
import Heos.FakeDevice (answering, answeringWith, commandPath, commandQuery, replyTo)
import Heos.Handler.Live (ProtocolError (..), runHeosLife)
import Heos.Media (ContainerId (..), Found (..), MediaId (..), Playable (..), SearchKind (..), SourceId (..))
import Heos.Player (PlayState (..), Player (..), PlayerId (..))
import Heos.Time (Seconds (..))
import Network.Socket (Family (AF_UNIX), SocketType (Stream), close, socketPair)
import System.FilePath ((</>))
import Test.Hspec

params :: ConnectionParams
params = ConnectionParams{commandTimeout = Seconds 1, heartbeatDelay = Seconds 3600, playerId = PlayerId 1, maxLineLength = 64 * 1024}

-- Run a program against the Live handler over a connection to a fake device
-- that answers with `reply`. Returns the result and the commands the device
-- saw on the commands socket, in order.
withLive :: (ByteString -> ByteString) -> Eff '[Heos, IOE] a -> IO (a, [ByteString])
withLive reply prog =
  withPair $ \(ourEvents, devEvents) ->
    withPair $ \(ourCommands, devCommands) -> do
      seen <- newTVarIO []
      let record line = atomically (modifyTVar' seen (line :))
      withAsync (answering (const (pure ())) devEvents) $ \_ ->
        withAsync (answeringWith reply record devCommands) $ \_ -> do
          result <- runConnection params (ConnectionSockets ourEvents ourCommands) $ \conn ->
            runEff (runHeosLife conn prog)
          commands <- reverse <$> readTVarIO seen
          pure (result, commands)
 where
  withPair = bracket (socketPair AF_UNIX Stream 0) (\(a, b) -> close a >> close b)

-- The commands the device saw, as (path, query without SEQUENCE).
sent :: [ByteString] -> [(ByteString, ByteString)]
sent = map (\l -> (commandPath l, dropSeq (commandQuery l)))
 where
  dropSeq q = BS.intercalate "&" [p | p <- BC.split '&' q, not ("SEQUENCE=" `BS.isPrefixOf` p)]

-- Answer from a table by command path; anything else gets the echo reply.
scripted :: [(ByteString, ByteString)] -> ByteString -> ByteString
scripted table line = fromMaybe (replyTo line) (lookup (commandPath line) table)

-- A success reply whose message carries the given query, the way get_volume
-- and get_play_state answer.
messageReply :: ByteString -> ByteString -> ByteString
messageReply path msg =
  "{\"heos\": {\"command\": \"" <> path <> "\", \"result\": \"success\", \"message\": \"" <> msg <> "\"}}\r\n"

-- A recorded reply as one wire line. The files are pretty-printed; only
-- "\r\n" ends a line on the wire, so stray "\r" must go.
fixture :: FilePath -> IO ByteString
fixture file = (<> "\r\n") . BC.filter (/= '\r') <$> BS.readFile ("test/fixtures/responses" </> file)

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

    it "setPlayState renders the state" $ do
      (_, cmds) <- withLive replyTo (setPlayState Pause)
      sent cmds `shouldBe` [("player/set_play_state", "pid=1&state=pause")]

    -- Easy to swap, and nothing but a test would notice.
    it "playNext and playPrevious are not mixed up" $ do
      (_, cmds) <- withLive replyTo (playNext >> playPrevious)
      map fst (sent cmds) `shouldBe` ["player/play_next", "player/play_previous"]

    it "enqueue of a track sends sid, cid, mid and the mode" $ do
      (_, cmds) <- withLive replyTo (enqueue AddToEnd (PlayTrack (SourceId 10) (ContainerId "SEARCHED_TRACKS-x") (MediaId "5")))
      sent cmds `shouldBe` [("browse/add_to_queue", "pid=1&sid=10&cid=SEARCHED_TRACKS-x&mid=5&aid=3")]

    it "enqueue of a container sends sid and cid, no mid" $ do
      (_, cmds) <- withLive replyTo (enqueue AddPlayNow (PlayContainer (SourceId 10) (ContainerId "77")))
      sent cmds `shouldBe` [("browse/add_to_queue", "pid=1&sid=10&cid=77&aid=1")]

    -- The first replaces the queue and starts, the rest line up behind it.
    it "playAll replaces the queue with the first item and appends the rest" $ do
      let item n = PlayContainer (SourceId 10) (ContainerId n)
      (_, cmds) <- withLive replyTo (playAll [item "a", item "b", item "c"])
      map snd (sent cmds) `shouldBe` ["pid=1&sid=10&cid=a&aid=4", "pid=1&sid=10&cid=b&aid=3", "pid=1&sid=10&cid=c&aid=3"]

  describe "replies coming back" $ do
    it "getVolume reads level from the message" $ do
      (v, _) <- withLive (scripted [("player/get_volume", messageReply "player/get_volume" "pid=1&level=15")]) getVolume
      v `shouldBe` 15

    it "getPlayState reads state from the message" $ do
      (s, _) <- withLive (scripted [("player/get_play_state", messageReply "player/get_play_state" "pid=1&state=play")]) getPlayState
      s `shouldBe` Play

    it "getPlayers decodes the payload of a recorded reply" $ do
      players <- fixture "get_players.json"
      (ps, _) <- withLive (scripted [("player/get_players", players)]) getPlayers
      map (.name) ps `shouldBe` ["Marantz MODEL 60n"]

    -- A reply the handler cannot make sense of is a ProtocolError, not a
    -- silent default: here the device answered get_volume without a level.
    it "a message without the expected key is a ProtocolError" $
      withLive replyTo getVolume `shouldThrow` isProtocolError

    it "a reply without payload where one is expected is a ProtocolError" $
      withLive replyTo getPlayers `shouldThrow` isProtocolError

  describe "search" $ do
    it "asks for the criteria, then searches with the matching scid, and returns playable tracks" $ do
      criteria <- fixture "search_criteria.json"
      results <- fixture "search_ok_computer.json"
      (found, cmds) <-
        withLive
          (scripted [("browse/get_search_criteria", criteria), ("browse/search", results)])
          (search (SourceId 10) Track "Ok Computer")
      sent cmds
        `shouldBe` [ ("browse/get_search_criteria", "sid=10")
                   , ("browse/search", "sid=10&search=Ok Computer&scid=3")
                   ]
      length found `shouldBe` 50
      [t | Found{target = Just t} <- found]
        `shouldSatisfy` all (\case PlayTrack (SourceId 10) (ContainerId "SEARCHED_TRACKS-Ok Computer") _ -> True; _ -> False)

    it "picks the scid the device gives for the kind, not a fixed number" $ do
      criteria <- fixture "search_criteria.json"
      results <- fixture "search_ok_computer.json"
      (_, cmds) <-
        withLive
          (scripted [("browse/get_search_criteria", criteria), ("browse/search", results)])
          (search (SourceId 10) Playlist "chill")
      map snd (sent cmds) `shouldBe` ["sid=10", "sid=10&search=chill&scid=6"]

    it "fails with a ProtocolError, without searching, when the source has no such criterion" $ do
      let noCriteria = "{\"heos\": {\"command\": \"browse/get_search_criteria\", \"result\": \"success\", \"message\": \"sid=3\"}, \"payload\": []}\r\n"
      withLive (scripted [("browse/get_search_criteria", noCriteria)]) (search (SourceId 3) Track "x")
        `shouldThrow` isProtocolError
