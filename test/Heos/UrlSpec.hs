-- it's ok in tests to use partial functions
{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Heos.UrlSpec (spec) where

import Data.Either (isLeft, isRight)
import Data.Text (Text)
import Data.Text qualified as T
import Heos.Url.Internal
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  ----------------------------------------------------------------------------
  -- Keys
  ----------------------------------------------------------------------------

  describe "makeParamKey" $ do
    it "accepts the keys HEOS actually uses" $
      mapM_
        (\k -> makeParamKey k `shouldSatisfy` isRight)
        ["pid", "level", "sid", "scid", "cid", "mid", "search", "range", "count"]

    it "rejects keys containing characters the protocol reserves" $
      mapM_
        (\k -> makeParamKey k `shouldSatisfy` isLeft)
        ["a=b", "a&b", "100%", "a?b", "a b"]

    -- Its own case, because emptiness needs its own check: T.all isAlphaNum ""
    -- is vacuously true — there is no element to refute it, so the universal
    -- statement holds. T.all alone is not enough here.
    it "rejects the empty key" $
      makeParamKey "" `shouldSatisfy` isLeft

    -- The reason we moved from Maybe to Either: the cause of rejection is
    -- observable. KeyFailure's constructors are not exported, so we compare
    -- the rendered message.
    it "says why it rejected the key" $ do
      makeParamKey "" `shouldBe` Left EmptyKey
      makeParamKey "a%b" `shouldBe` Left (IllegalKey "a%b")

    -- renderProblem is what ends up in the log, so pin the wording too.
    it "renders the reason as text for the log" $ do
      renderProblem EmptyKey `shouldBe` "Empty key"
      renderProblem (IllegalKey "a%b") `shouldBe` "Illegal symbols in key: a%b"

    -- Underscore was once rejected here and this test pinned that as the rule;
    -- HEOS does use '_' in keys, so the rule was loosened.
    it "accepts underscores in a key" $
      mapM_
        (\k -> makeParamKey k `shouldBe` Right (makeKey k))
        ["set_volume", "_a", "a_", "a__b", "_"]

    it "still rejects other punctuation next to an underscore" $
      mapM_
        (\k -> makeParamKey k `shouldBe` Left (IllegalKey k))
        ["a_-b", "a_.b", "a_ b", "a_=b"]

    it "Unicode counts as a letter" $ do
      -- ну и слово придумал Клод
      makeParamKey "пид" `shouldSatisfy` isRight

  ----------------------------------------------------------------------------
  -- Rendering
  ----------------------------------------------------------------------------

  describe "renderValue" $ do
    it "leaves harmless text untouched" $
      renderValue "Pink Floyd" `shouldBe` "Pink Floyd"

    it "escapes the characters the protocol reserves" $ do
      renderValue "AC/DC & Queen" `shouldBe` "AC/DC %26 Queen"
      renderValue "a=b" `shouldBe` "a%3Db"
      renderValue "100%" `shouldBe` "100%25"

    it "does not re-escape a percent it just inserted" $
      renderValue "&" `shouldBe` "%26"

  describe "renderQuery" $ do
    it "an empty list renders as an empty string" $
      renderQuery [] `shouldBe` ""

    it "a single pair carries no separator" $
      renderQuery [makePair "pid" "1"] `shouldBe` "pid=1"

    it "joins pairs with &" $
      renderQuery [makePair "pid" "1", makePair "level" "20"]
        `shouldBe` "pid=1&level=20"

    -- The key is not escaped and must not be: makeParamKey already guaranteed
    -- there is nothing in it to escape.
    it "escapes the value" $
      renderQuery [makePair "search" "AC/DC & Queen"]
        `shouldBe` "search=AC/DC %26 Queen"

  describe "renderCommandUrl" $ do
    it "attaches parameters to the address with ?" $
      renderCommandUrl "heos://player/set_volume" [makePair "pid" "1", makePair "level" "20"]
        `shouldBe` "heos://player/set_volume?pid=1&level=20"

    it "omits the ? when there are no parameters" $
      renderCommandUrl "heos://player/set_volume" []
        `shouldBe` "heos://player/set_volume"

  -- renderQuery only accepts [UrlPair], so it can never emit a flag or a
  -- reject. renderUrlParams covers the other direction: what the DEVICE sends.
  -- Its real caller is the fake handler, which has to impersonate a device.
  describe "renderUrlParams" $ do
    it "an empty list renders as an empty string" $
      renderUrlParams [] `shouldBe` ""

    it "renders a flag verbatim" $
      renderUrlParams [UrlFlag "command under process"]
        `shouldBe` "command under process"

    it "renders a pair with its value escaped" $
      renderUrlParams [UrlKeyValue (makePair "search" "AC/DC & Queen")]
        `shouldBe` "search=AC/DC %26 Queen"

    -- The only sensible rendering of a reject: the chunk it kept. A KeyFailure
    -- is this parser's verdict, the protocol has no notion of it.
    it "renders a reject as the original chunk it kept" $
      renderUrlParams [UrlFailure "a%b=x" (IllegalKey "a%b")] `shouldBe` "a%b=x"

    it "rebuilds a whole device message unchanged" $
      let msg = "command under process&sid=10&search=Ok Computer&scid=3"
       in renderUrlParams (decodeQuery msg) `shouldBe` msg

  ----------------------------------------------------------------------------
  -- Parsing
  ----------------------------------------------------------------------------

  describe "decodeValue" $ do
    it "expands escape sequences" $ do
      decodeValue "AC/DC %26 Queen" `shouldBe` "AC/DC & Queen"
      decodeValue "a%3Db" `shouldBe` "a=b"
      decodeValue "100%25" `shouldBe` "100%"

    -- All three reserved characters in one string, spelled out as a named
    -- example. The property below covers this shape on every run (about half
    -- the generated cases contain all three), but the worked example is the
    -- one quoted in docs/ROADMAP.md, so it earns a test of its own.
    it "round-trips a value using all three reserved characters at once" $ do
      renderValue "a&b=c%d" `shouldBe` "a%26b%3Dc%25d"
      decodeValue (renderValue "a&b=c%d") `shouldBe` "a&b=c%d"

  describe "splitPair" $ do
    it "a chunk with = becomes a pair and its value is unescaped" $ do
      splitPair "pid=1" `shouldBe` UrlKeyValue (makePair "pid" "1")
      splitPair "q=a%26b" `shouldBe` UrlKeyValue (makePair "q" "a&b")

    it "a chunk without = becomes a flag, and flags are not validated" $ do
      splitPair "command under process"
        `shouldBe` UrlFlag "command under process"
      splitPair "100% junk" `shouldBe` UrlFlag "100% junk"

    it "an empty value still makes a pair, not a flag" $
      splitPair "pid=" `shouldBe` UrlKeyValue (makePair "pid" "")

    -- Split on the FIRST '=', the rest goes into the value. renderQuery would
    -- never produce this (it escapes '=' in values), so parsing is more liberal
    -- than rendering — which is the right asymmetry for someone else's input.
    it "extra = characters go into the value instead of breaking the parse" $
      splitPair "a=b=c" `shouldBe` UrlKeyValue (makePair "a" "b=c")

    -- A bad key no longer masquerades as a device message: it has its own
    -- constructor, and it keeps the ORIGINAL chunk verbatim, including the
    -- value that used to be dropped.
    it "a bad key becomes a UrlFailure carrying the original chunk and the reason" $ do
      splitPair "a%b=x" `shouldBe` UrlFailure "a%b=x" (IllegalKey "a%b")
      splitPair "=z" `shouldBe` UrlFailure "=z" EmptyKey

  describe "decodeQuery" $ do
    it "parses an ordinary parameter string" $
      decodeQuery "pid=1&level=20"
        `shouldBe` [UrlKeyValue (makePair "pid" "1"), UrlKeyValue (makePair "level" "20")]

    it "a flag mixed with pairs - the device response quoted in Types.hs" $
      decodeQuery "command under process&sid=10&search=Ok Computer&scid=3"
        `shouldBe` [ UrlFlag "command under process"
                   , UrlKeyValue (makePair "sid" "10")
                   , UrlKeyValue (makePair "search" "Ok Computer")
                   , UrlKeyValue (makePair "scid" "3")
                   ]

    -- This is why the list stays heterogeneous: a Maybe Text would hold only
    -- the first flag and the rest would have to be silently dropped.
    it "several flags can arrive, and all of them survive" $
      decodeQuery "command under process&sid=10&partial"
        `shouldBe` [ UrlFlag "command under process"
                   , UrlKeyValue (makePair "sid" "10")
                   , UrlFlag "partial"
                   ]

    it "a broken parameter neither disturbs its neighbours nor poses as a flag" $ do
      let ps = decodeQuery "command under process&sid=10&a%b=x"
      length ps `shouldBe` 3
      head ps `shouldBe` UrlFlag "command under process"
      ps !! 1 `shouldBe` UrlKeyValue (makePair "sid" "10")
      ps !! 2 `shouldBe` UrlFailure "a%b=x" (IllegalKey "a%b")

    it "an empty parameter string yields an empty list" $
      decodeQuery "" `shouldBe` []

    it "stray separators do not produce empty flags" $ do
      decodeQuery "pid=1&" `shouldBe` [UrlKeyValue (makePair "pid" "1")]
      decodeQuery "&pid=1" `shouldBe` [UrlKeyValue (makePair "pid" "1")]
      decodeQuery "pid=1&&level=20"
        `shouldBe` [UrlKeyValue (makePair "pid" "1"), UrlKeyValue (makePair "level" "20")]

  describe "decodeCommandUrl" $ do
    it "splits an address with parameters" $
      decodeCommandUrl "heos://player/set_volume?pid=1&level=20"
        `shouldBe` UrlResponse
          "heos://player/set_volume"
          [UrlKeyValue (makePair "pid" "1"), UrlKeyValue (makePair "level" "20")]

    it "returns the address alone when there are no parameters" $
      decodeCommandUrl "heos://player/set_volume"
        `shouldBe` UrlResponse "heos://player/set_volume" []

  ----------------------------------------------------------------------------
  -- Properties
  ----------------------------------------------------------------------------

  describe "properties" $ do
    it "decodeValue . renderValue == id" $
      forAll wireText $ \t ->
        decodeValue (renderValue t) === t

    it "renderValue leaves no bare & or = in a value" $
      forAll wireText $ \t ->
        T.all (`notElem` ['&', '=']) (renderValue t)

    it "decodeQuery . renderQuery == id" $
      forAll (listOf1 wirePair) $ \ps ->
        classify (any (T.any (`elem` ['%', '&', '=']) . val) ps) "with reserved characters" $
          decodeQuery (renderQuery ps) === (UrlKeyValue <$> ps)

    it "decodeCommandUrl . renderCommandUrl == id" $
      forAll ((,) <$> wireAddress <*> listOf wirePair) $ \(addr, ps) ->
        decodeCommandUrl (renderCommandUrl addr ps)
          === UrlResponse addr (UrlKeyValue <$> ps)

    it "parsing never emits an empty flag" $
      forAll wireQueryText $ \q ->
        UrlFlag "" `notElem` decodeQuery q

    it "never more parameters than there are chunks between &" $
      forAll wireQueryText $ \q ->
        length (decodeQuery q) <= length (T.splitOn "&" q)

    -- The central ParamKey invariant: whatever arrives on the wire, every makePair
    -- in the output is one the parser would accept again. A key that slipped
    -- past makeParamKey (say, with '&' or '=' in it) would not survive being
    -- rendered and parsed back, so this checks the keys without looking inside.
    it "every pair in the output survives a render/parse round trip" $
      forAll wireQueryText $ \q ->
        all (\p -> decodeQuery (renderQuery [p]) == [UrlKeyValue p]) [p | UrlKeyValue p <- decodeQuery q]

    -- This used to fail: the error text ended up in a UrlFlag and showed up as
    -- "a flag that was never in the input". The separate constructor fixed it.
    it "every flag in the output appeared as a chunk in the input" $
      forAll wireQueryText $ \q ->
        all (`elem` T.splitOn "&" q) [f | UrlFlag f <- decodeQuery q]

    -- Same for rejects: a UrlFailure must carry the original chunk verbatim,
    -- otherwise there is nothing useful to put in the log.
    it "every UrlFailure carries its original chunk" $
      forAll wireQueryText $ \q ->
        all (`elem` T.splitOn "&" q) [c | UrlFailure c _ <- decodeQuery q]

    -- The two renderers must not drift apart: on the pairs they both accept,
    -- they have to agree character for character.
    it "renderUrlParams agrees with renderQuery on plain pairs" $
      forAll (listOf1 wirePair) $ \ps ->
        renderUrlParams (UrlKeyValue <$> ps) === renderQuery ps

    -- Coming from the WIRE side, rendering back is a normaliser, not an
    -- identity: decodeQuery drops empty chunks, so "c&" can never come back.
    -- Idempotence is the honest law, and it is the only one that exercises
    -- flags and rejects — the two thirds of UrlParam the round-trips above
    -- can never reach, because renderQuery cannot produce them.
    it "decodeQuery . renderUrlParams . decodeQuery == decodeQuery" $
      forAll wireQueryText $ \q ->
        decodeQuery (renderUrlParams (decodeQuery q)) === decodeQuery q

    it "every chunk yields exactly one parameter - nothing is lost" $
      forAll wireQueryText $ \q ->
        length (decodeQuery q) === length (filter (/= "") (T.splitOn "&" q))

--------------------------------------------------------------------------------
-- Helpers and generators
--------------------------------------------------------------------------------

-- QuickCheck has no Arbitrary instance for Text. The alphabet is deliberately
-- narrow and stuffed with reserved characters and fragments of escape
-- sequences: a naive `T.pack <$> arbitrary` would essentially never emit '%',
-- '2', '6' in a row, and those are precisely the inputs that break the codec.
wireText :: Gen Text
wireText = T.pack <$> listOf (elements "%&=?abc2356D ")

-- A query string "as the device would send it": junk, flags, empty chunks and
-- bad keys. listOf1, because "" is a separate contract covered by an example.
wireQueryText :: Gen Text
wireQueryText = T.pack <$> listOf1 (elements "&=%abc12 ")

wireAddress :: Gen Text
wireAddress = T.pack <$> listOf1 (elements "abcdeopqrstuvxyz/:_")

wirePair :: Gen UrlPair
wirePair = makePair <$> wireKey <*> wireText
 where
  wireKey = T.pack <$> listOf1 (elements "abcdepilv12_")
