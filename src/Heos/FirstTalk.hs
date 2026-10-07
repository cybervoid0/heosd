module Heos.FirstTalk (firstTalk) where

import Control.Concurrent.Async (race_)
import Control.Exception (bracket, finally)
import Data.ByteString (ByteString)
import Data.Foldable (forM_)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Heos.Command (Command (..), renderCommand)
import Heos.Connection (splitLines)
import Heos.Discovery (discover)
import Heos.Player (OnOff (On), PlayState (Pause, Play), PlayerId (PlayerId))
import Network.Socket (Family (AF_INET), Socket, SocketType (Stream), close, connect, defaultProtocol, socket)
import Network.Socket.ByteString (recv, sendAll)

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
    -- параллельный запуск вывода на экран и пульта управления в отдельных потоках
    -- оба потока закрываются при завершении одного из них (нажатие "q")
    race_
      (readLoop sock "" `finally` putStrLn "reader: stopped")
      (remote sock `finally` putStrLn "remote: stopped")

-- Этап 13 - Чтение в цикле
readLoop :: Socket -> ByteString -> IO ()
readLoop sock buffer = do
  chunk <- recv sock 4096
  let (complete, rest) = splitLines (buffer <> chunk)
  forM_ complete print
  readLoop sock rest

-- Этап 14 - Потоки
remote :: Socket -> IO ()
remote sock = do
  line <- getLine
  case line of
    "q" -> pure ()
    _ -> do
      case keyToCommand line of
        Just cmd -> sendAll sock (renderCommand 0 cmd)
        Nothing -> putStrLn "команды: + - play pause q"
      remote sock

keyToCommand :: String -> Maybe Command
keyToCommand cmd = case cmd of
  "+" -> Just $ VolumeUp pid 1
  "-" -> Just $ VolumeDown pid 1
  "play" -> Just $ SetPlayState pid Play
  "pause" -> Just $ SetPlayState pid Pause
  _ -> Nothing
 where
  pid = PlayerId (-1245882071)
