module Heos.Discovery where

import Network.Socket (AddrInfo (..), Family (..), HostName, SocketType (..), defaultHints, getAddrInfo)

resolve :: HostName -> IO AddrInfo
resolve host = do
  let hints =
        defaultHints
          { addrSocketType = Stream
          , addrFamily = AF_INET
          }
  (addr : _) <- getAddrInfo (Just hints) (Just host) (Just "1255")
  pure addr
