module Heos.ConnectionSpec (spec) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Heos.Connection.Internal (splitLines)
import Test.Hspec
import Test.QuickCheck

-- Bytes that make splitting interesting: plain letters plus both halves of
-- the delimiter, so "\r\n", a lone "\r" and a lone "\n" all show up.
genBytes :: Gen ByteString
genBytes = BC.pack <$> listOf (elements "ab\r\n")

-- Cut a string at the given positions, the way recv hands out chunks of
-- arbitrary size.
chunksAt :: [Int] -> ByteString -> [ByteString]
chunksAt [] bs = [bs]
chunksAt (n : ns) bs = let (h, t) = BS.splitAt n bs in h : chunksAt ns t

-- Feed chunks one by one, carrying the remainder, as the read loop of stage 13
-- will do.
feed :: [ByteString] -> ([ByteString], ByteString)
feed = foldl step ([], "")
 where
  step (done, buffer) chunk =
    let (complete, rest) = splitLines (buffer <> chunk)
     in (done <> complete, rest)

spec :: Spec
spec = describe "splitLines" $ do
  -- The three checks named in the roadmap.
  it "keeps a chunk without a delimiter as the remainder" $
    splitLines "{\"heos\": {\"comm" `shouldBe` ([], "{\"heos\": {\"comm")

  it "returns both lines from a chunk with two complete lines" $
    splitLines "one\r\ntwo\r\n" `shouldBe` (["one", "two"], "")

  it "splits off the unfinished tail of a chunk that ends mid-line" $
    splitLines "one\r\ntwo\r\nthr" `shouldBe` (["one", "two"], "thr")

  it "returns nothing for an empty chunk" $
    splitLines "" `shouldBe` ([], "")

  -- "\r" alone is half a delimiter: the "\n" may be in the next chunk.
  it "keeps a trailing \\r in the remainder" $
    splitLines "one\r" `shouldBe` ([], "one\r")

  it "doesn't treat a bare \\n or \\r as a delimiter" $
    splitLines "a\nb\rc\r\n" `shouldBe` (["a\nb\rc"], "")

  -- An empty line is still a line. HEOS doesn't send them, but splitLines
  -- shouldn't silently drop data either; skipping empties is the caller's call.
  it "reports an empty line between two delimiters" $
    splitLines "one\r\n\r\ntwo\r\n" `shouldBe` (["one", "", "two"], "")

  it "splits a real get_players reply with the start of the next one glued on" $
    splitLines (getPlayers <> "\r\n{\"heos\": {\"command\": \"event/")
      `shouldBe` ([getPlayers], "{\"heos\": {\"command\": \"event/")

  describe "properties" $ do
    it "loses nothing: lines and remainder joined back give the input" $
      forAll genBytes $ \s ->
        let (complete, rest) = splitLines s
         in BS.intercalate "\r\n" (complete <> [rest]) === s

    it "never leaves a delimiter inside a line or the remainder" $
      forAll genBytes $ \s ->
        let (complete, rest) = splitLines s
         in not (any (BS.isInfixOf "\r\n") (rest : complete))

    -- The reason splitLines exists: however recv cuts the stream, carrying the
    -- remainder over must give the same lines as reading it all at once.
    it "gives the same result however the input is cut into chunks" $
      forAll genBytes $ \s ->
        forAll (listOf (choose (0, 5))) $ \cuts ->
          feed (chunksAt cuts s) === splitLines s
 where
  getPlayers =
    "{\"heos\": {\"command\": \"player/get_players\", \"result\": \"success\", \"message\": \"\"}, \"payload\": [{\"name\": \"Marantz MODEL 60n\", \"pid\": -1245882071}]}"
