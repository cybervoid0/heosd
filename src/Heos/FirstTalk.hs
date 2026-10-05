module Heos.FirstTalk where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Heos.Command (Command (GetPlayers, RegisterForChangeEvents), renderCommand)
import Heos.Connection (splitLines)
import Heos.Discovery (discover)
import Heos.Player (OnOff (On))
import Network.Socket (Family (AF_INET), Socket, SocketType (Stream), connect, defaultProtocol, socket)
import Network.Socket.ByteString (recv, sendAll)

-- Этап 11
firstTalk :: IO ()
firstTalk = discover >>= either print talk
 where
  talk (addr :| _) = do
    sock <- socket AF_INET Stream defaultProtocol
    connect sock addr
    sendAll sock (renderCommand 1 GetPlayers)
    bytes <- recv sock 4096
    print bytes

    sendAll sock (renderCommand 2 (RegisterForChangeEvents On))
    readLoop sock ""

-- Этап 13
readLoop :: Socket -> ByteString -> IO ()
readLoop sock buffer = do
  chunk <- recv sock 4096
  let (complete, rest) = splitLines (buffer <> chunk)
  mapM_ print complete
  readLoop sock rest