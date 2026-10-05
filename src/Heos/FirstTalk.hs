module Heos.FirstTalk where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Heos.Discovery (discover)
import Network.Socket (Family (AF_INET), SocketType (Stream), connect, defaultProtocol, socket)
import Network.Socket.ByteString (recv, sendAll)

firstTalk :: IO ()
firstTalk = discover >>= either print talk
 where
  talk (addr :| _) = do
    sock <- socket AF_INET Stream defaultProtocol
    connect sock addr
    sendAll sock "heos://player/get_players\r\n"
    bytes <- recv sock 4096
    print bytes