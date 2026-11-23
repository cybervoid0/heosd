-- | 'Heos.Logger' on its own: what a line looks like, what the threshold
--   lets through, how loggers are wrapped and combined. What the library
--   says through the logger is in the specs of the modules that say it.
module Heos.LoggerSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_)
import Control.Exception (ErrorCall (..), bracket, throwIO)
import Control.Monad (forM_)
import Data.Aeson (eitherDecode)
import Data.Char (isDigit)
import Data.Either (isLeft)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf, sort)
import Heos.Logger (Logger (..), Severity (..), capturing, logged, newLogger, scoped, timed)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (Handle, hClose, openTempFile)
import Test.Hspec

-- Run the action over a handle and return the lines it wrote there. The
-- handle is a temporary file: the logger takes any 'Handle', and a file
-- can be read back.
written :: (Handle -> IO ()) -> IO [String]
written act = do
  dir <- getTemporaryDirectory
  bracket (openTempFile dir "heosd-logger.log") (removeFile . fst) $ \(path, h) -> do
    act h
    hClose h
    contents <- readFile path
    length contents `seq` pure (lines contents)

-- What a logger with this threshold writes for these messages.
linesAt :: Severity -> [(Severity, String)] -> IO [String]
linesAt threshold messages = written $ \h -> do
  logger <- newLogger h threshold
  forM_ messages (uncurry logger.logAt)

-- A line without its time stamp: the severity and the message.
afterStamp :: String -> String
afterStamp = unwords . drop 2 . words

-- Digits become 'd', the rest stays: the shape of a time stamp.
shape :: String -> String
shape = map (\c -> if isDigit c then 'd' else c)

severities :: [Severity]
severities = [Debug, Info, Warn, Error]

-- A logger that appends its tag to a shared list, to see who wrote when.
tagging :: IO (String -> Logger, IO [String])
tagging = do
  ref <- newIORef []
  pure (\tag -> Logger $ \_ msg -> modifyIORef' ref ((tag <> ":" <> msg) :), reverse <$> readIORef ref)

spec :: Spec
spec = do
  describe "Severity" $ do
    it "orders from Debug up to Error" $
      sort [Error, Debug, Warn, Info] `shouldBe` severities

    it "reads the lower-case names the config file uses" $
      eitherDecode "[\"debug\", \"info\", \"warn\", \"error\"]" `shouldBe` Right severities

    it "refuses an unknown name and lists the known ones" $
      case eitherDecode @Severity "\"verbose\"" of
        Right sev -> expectationFailure ("parsed as " <> show sev)
        Left err -> forM_ ["verbose", "debug", "info", "warn", "error"] $ \word -> err `shouldSatisfy` (word `isInfixOf`)

    it "refuses a name in another case" $
      eitherDecode @Severity "\"INFO\"" `shouldSatisfy` isLeft

    it "refuses anything but a string" $
      eitherDecode @Severity "2" `shouldSatisfy` isLeft

  describe "newLogger" $ do
    it "writes one line per message: time stamp, severity in capitals, the message" $ do
      [line] <- linesAt Debug [(Info, "signed in as listener@example.com")]
      shape (unwords (take 2 (words line))) `shouldBe` "dddd-dd-dd dd:dd:dd"
      afterStamp line `shouldBe` "INFO signed in as listener@example.com"

    it "names every severity" $ do
      written' <- linesAt Debug [(sev, "x") | sev <- severities]
      map afterStamp written' `shouldBe` ["DEBUG x", "INFO x", "WARN x", "ERROR x"]

    it "keeps the messages in the order they were logged" $ do
      written' <- linesAt Debug [(Info, show n) | n <- [1 :: Int .. 20]]
      map afterStamp written' `shouldBe` ["INFO " <> show n | n <- [1 :: Int .. 20]]

    it "drops what is below the threshold and keeps the threshold itself" $ do
      written' <- linesAt Warn [(sev, show sev) | sev <- severities]
      map afterStamp written' `shouldBe` ["WARN Warn", "ERROR Error"]

    it "writes everything at Debug" $
      linesAt Debug [(sev, "x") | sev <- severities] >>= (`shouldSatisfy` ((== 4) . length))

    it "writes only errors at Error" $ do
      written' <- linesAt Error [(sev, "x") | sev <- severities]
      map afterStamp written' `shouldBe` ["ERROR x"]

    -- A dropped message costs nothing: it is never looked at. A debug
    -- line may 'show' something big without slowing the daemon down.
    it "does not evaluate a message it drops" $
      linesAt Info [(Debug, error "the dropped message was evaluated")] `shouldReturn` []

    it "keeps a message with spaces and punctuation as it is" $ do
      [line] <- linesAt Debug [(Warn, "sign-in refused: (Just 10,Just \"User not found\"), streaming is off")]
      afterStamp line `shouldBe` "WARN sign-in refused: (Just 10,Just \"User not found\"), streaming is off"

    -- The reader, the heartbeats and the commands log from their own
    -- threads. Every line must come out whole.
    it "does not mix up lines written from several threads at once" $ do
      let writers = [1 :: Int .. 8]
          perWriter = [1 :: Int .. 50]
          message w n = "writer " <> show w <> " line " <> show n <> " " <> replicate 60 '.'
      written' <- written $ \h -> do
        logger <- newLogger h Debug
        forConcurrently_ writers $ \w -> forM_ perWriter $ \n -> logger.logAt Info (message w n)
      sort (map afterStamp written') `shouldBe` sort ["INFO " <> message w n | w <- writers, n <- perWriter]

    it "keeps each writer's own lines in order" $ do
      written' <- written $ \h -> do
        logger <- newLogger h Debug
        forConcurrently_ ["a", "b", "c"] $ \w -> forM_ [1 :: Int .. 30] $ \n -> logger.logAt Info (w <> " " <> show n)
      forM_ ["a", "b", "c"] $ \w ->
        [l | l <- map afterStamp written', ("INFO " <> w <> " ") `isInfixOf` l] `shouldBe` ["INFO " <> w <> " " <> show n | n <- [1 :: Int .. 30]]

  describe "scoped" $ do
    it "puts the name in front of the message" $ do
      (spy, seen) <- capturing
      (scoped "heos" spy).logAt Info "connected"
      seen `shouldReturn` [(Info, "heos: connected")]

    it "leaves the severity alone" $ do
      (spy, seen) <- capturing
      forM_ severities $ \sev -> (scoped "avr" spy).logAt sev "x"
      map fst <$> seen `shouldReturn` severities

    it "leaves the logger it wraps as it was" $ do
      (spy, seen) <- capturing
      (scoped "heos" spy).logAt Info "one"
      spy.logAt Info "two"
      seen `shouldReturn` [(Info, "heos: one"), (Info, "two")]

    -- Each wrapper hands its result to the logger it wraps, so the name
    -- given first, the one next to the sink, stays in front: a connection
    -- handed `scoped "heos" logger` can scope it further for its reader.
    it "nests: the name given first stays in front" $ do
      (spy, seen) <- capturing
      (scoped "reader" (scoped "heos" spy)).logAt Warn "stray reply"
      seen `shouldReturn` [(Warn, "heos: reader: stray reply")]

    it "is still held back by the threshold of the logger it wraps" $ do
      written' <- written $ \h -> do
        logger <- newLogger h Warn
        (scoped "heos" logger).logAt Info "quiet"
        (scoped "heos" logger).logAt Error "loud"
      map afterStamp written' `shouldBe` ["ERROR heos: loud"]

  describe "the silent logger (mempty)" $ do
    it "takes any message and does nothing" $
      forM_ severities (\sev -> (mempty :: Logger).logAt sev "nobody hears this") `shouldReturn` ()

    it "does not evaluate the message" $
      (mempty :: Logger).logAt Error (error "the silent logger evaluated its message") `shouldReturn` ()

  describe "two loggers combined (<>)" $ do
    it "hands every message to both, the left one first" $ do
      (tagged, order) <- tagging
      (tagged "left" <> tagged "right").logAt Info "m"
      order `shouldReturn` ["left:m", "right:m"]

    it "gives both the same severity and message" $ do
      (one, seenOne) <- capturing
      (two, seenTwo) <- capturing
      (one <> two).logAt Warn "link dropped"
      seenOne `shouldReturn` [(Warn, "link dropped")]
      seenTwo `shouldReturn` [(Warn, "link dropped")]

    it "changes nothing when combined with the silent one, on either side" $ do
      (spy, seen) <- capturing
      (mempty <> spy).logAt Info "a"
      (spy <> mempty).logAt Info "b"
      seen `shouldReturn` [(Info, "a"), (Info, "b")]

    -- Each side keeps its own threshold: one sink can take everything
    -- while the other takes only what matters.
    it "lets each side keep its own threshold" $ do
      (spy, seen) <- capturing
      written' <- written $ \h -> do
        strict <- newLogger h Error
        forM_ severities $ \sev -> (strict <> spy).logAt sev "x"
      map afterStamp written' `shouldBe` ["ERROR x"]
      map fst <$> seen `shouldReturn` severities

  describe "capturing" $ do
    it "starts empty" $ do
      (_, seen) <- capturing
      seen `shouldReturn` []

    it "remembers every message with its severity, oldest first" $ do
      (spy, seen) <- capturing
      spy.logAt Debug "one"
      spy.logAt Error "two"
      spy.logAt Info "three"
      seen `shouldReturn` [(Debug, "one"), (Error, "two"), (Info, "three")]

    it "can be read more than once, and keeps collecting" $ do
      (spy, seen) <- capturing
      spy.logAt Info "one"
      seen `shouldReturn` [(Info, "one")]
      spy.logAt Info "two"
      seen `shouldReturn` [(Info, "one"), (Info, "two")]

    it "loses nothing when several threads log at once" $ do
      (spy, seen) <- capturing
      forConcurrently_ [1 :: Int .. 8] $ \w -> forM_ [1 :: Int .. 100] $ \n -> spy.logAt Info (show (w, n))
      sort . map snd <$> seen `shouldReturn` sort [show (w, n) | w <- [1 :: Int .. 8], n <- [1 :: Int .. 100]]

    it "keeps two spies apart" $ do
      (one, seenOne) <- capturing
      (_, seenTwo) <- capturing
      one.logAt Info "only here"
      seenOne `shouldReturn` [(Info, "only here")]
      seenTwo `shouldReturn` []

  -- 'logged' is what puts every command into the log: the action runs
  -- once, its outcome is noted with the name it was given, and the caller
  -- gets exactly what the action alone would have given, result or
  -- exception. The wording is not pinned, only what a line must carry.
  describe "logged" $ do
    it "returns what the action returns" $ do
      (spy, _) <- capturing
      logged spy "get_volume" (pure (15 :: Int)) `shouldReturn` 15

    it "runs the action exactly once" $ do
      (spy, _) <- capturing
      runs <- newIORef (0 :: Int)
      logged spy "get_volume" (modifyIORef' runs (+ 1))
      readIORef runs `shouldReturn` 1

    it "notes an action that went well once, below Warn, by its name" $ do
      (spy, seen) <- capturing
      logged spy "get_volume" (pure ())
      entries <- seen
      length entries `shouldBe` 1
      entries `shouldSatisfy` all (\(sev, msg) -> sev < Warn && "get_volume" `isInfixOf` msg)

    it "complains once about an action that failed, with its name and the reason" $ do
      (spy, seen) <- capturing
      logged spy "set_volume" (throwIO (ErrorCall "level out of range")) `shouldThrow` anyErrorCall
      entries <- seen
      length entries `shouldBe` 1
      entries `shouldSatisfy` all (\(sev, msg) -> sev >= Warn && "set_volume" `isInfixOf` msg && "level out of range" `isInfixOf` msg)

    it "hands the caller the very exception the action threw" $ do
      (spy, _) <- capturing
      logged spy "set_volume" (throwIO (ErrorCall "level out of range")) `shouldThrow` errorCall "level out of range"
      logged spy "read" (ioError (userError "boom")) `shouldThrow` anyIOException

    it "runs the action once even when it fails" $ do
      (spy, _) <- capturing
      runs <- newIORef (0 :: Int)
      logged spy "x" (modifyIORef' runs (+ 1) >> throwIO (ErrorCall "no")) `shouldThrow` anyErrorCall
      readIORef runs `shouldReturn` 1

    it "gives the same result and the same exception with the silent logger" $ do
      logged mempty "get_volume" (pure (15 :: Int)) `shouldReturn` 15
      logged mempty "set_volume" (throwIO (ErrorCall "no")) `shouldThrow` errorCall "no"

    it "says how long a slow action took, in the line about it" $ do
      (spy, seen) <- capturing
      logged spy "slow" (threadDelay 60000)
      [(_, msg)] <- seen
      -- some number of at least two digits: 60 ms or more
      [w | w <- words msg, all isDigit w, length w >= 2] `shouldSatisfy` (not . null)

  describe "timed" $ do
    it "returns what the action returns" $
      fst <$> timed (pure (42 :: Int)) `shouldReturn` 42

    it "measures how long the action took, in whole milliseconds" $ do
      (_, ms) <- timed (threadDelay 50000)
      ms `shouldSatisfy` (>= 50)
      ms `shouldSatisfy` (< 2000)

    it "reports next to nothing for an action that does nothing" $ do
      (_, ms) <- timed (pure ())
      ms `shouldSatisfy` (>= 0)
      ms `shouldSatisfy` (< 50)

    it "lets the action's exception through" $
      timed (ioError (userError "boom")) `shouldThrow` anyIOException
