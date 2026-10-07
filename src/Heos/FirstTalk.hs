module Heos.FirstTalk (firstTalk) where

import Control.Concurrent.Async (race_)
import Control.Concurrent.STM
import Control.Exception (bracket, finally)
import Data.Aeson (eitherDecodeStrict)
import Data.ByteString (ByteString)
import Data.Foldable (forM_)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as T
import Heos.Command (Command (..), renderCommand)
import Heos.Connection (splitLines)
import Heos.Discovery (discover)
import Heos.Envelope (Envelope (..), HeosEvent (..), IncomingEnvelope (..), classifyEnvelope)
import Heos.Player (OnOff (On), PlayState (Pause, Play), PlayerId (PlayerId))
import Heos.Url (lookupParam)
import Network.Socket (Family (AF_INET), Socket, SocketType (Stream), close, connect, defaultProtocol, socket)
import Network.Socket.ByteString (recv, sendAll)
import Text.Read (readMaybe)

-- Этап 11
firstTalk :: IO ()
firstTalk =
  discover >>= \case
    Left err -> print err
    Right addr -> bracket open (\s -> close s >> putStrLn "socket: closed") (`talk` addr)
 where
  open = socket AF_INET Stream defaultProtocol
  talk sock (addr :| _) = do
    connect sock addr
    sendAll sock (renderCommand 1 GetPlayers)
    bytes <- recv sock 4096
    print bytes

    sendAll sock (renderCommand 2 (RegisterForChangeEvents On))
    -- ячейка для хранения громкости
    volumeRef <- newTVarIO (Nothing :: Maybe Int)
    -- параллельный запуск вывода на экран и пульта управления в отдельных потоках
    -- оба потока закрываются при завершении одного из них (нажатие "q")
    race_
      (readLoop volumeRef sock "" `finally` putStrLn "reader: stopped")
      (remote volumeRef 0 sock `finally` putStrLn "remote: stopped")

-- Этап 13 - Чтение в цикле
readLoop :: TVar (Maybe Int) -> Socket -> ByteString -> IO ()
readLoop volumeRef sock buffer = do
  chunk <- recv sock 4096
  let (complete, rest) = splitLines (buffer <> chunk)
  forM_ complete $ \bs -> do
    let eitherIncEnv = classifyEnvelope <$> eitherDecodeStrict @Envelope bs
    case eitherIncEnv of
      Right (IncomingEvent (HeosEvent "player_volume_changed" params) _) -> do
        let mbVolume = lookupParam "level" params >>= readMaybe @Int . T.unpack
        -- Этап 15 - Изменяемые ячейки
        atomically $ writeTVar volumeRef mbVolume
      _ -> print bs
  readLoop volumeRef sock rest

-- Этап 14 - Потоки
remote :: TVar (Maybe Int) -> Int -> Socket -> IO ()
remote volumeRef seqVal sock = do
  line <- getLine
  case line of
    "q" -> pure ()
    "v" -> do
      volume <- readTVarIO volumeRef
      putStrLn (maybe "Volume: unknown" (("Volume: " <>) . show) volume)
      remote volumeRef seqVal sock
    _ -> do
      case keyToCommand line of
        Just cmd -> sendAll sock (renderCommand seqVal cmd)
        Nothing -> putStrLn "команды: + - play pause q v"
      remote volumeRef (seqVal + 1) sock

keyToCommand :: String -> Maybe Command
keyToCommand cmd = case cmd of
  "+" -> Just $ VolumeUp pid 1
  "-" -> Just $ VolumeDown pid 1
  "play" -> Just $ SetPlayState pid Play
  "pause" -> Just $ SetPlayState pid Pause
  _ -> Nothing
 where
  pid = PlayerId (-1245882071)
