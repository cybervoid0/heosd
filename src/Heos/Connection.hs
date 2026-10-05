module Heos.Connection (splitLines) where

import Heos.Connection.Internal

{- live connection: threads, queue, heartbeat -}
{-
The hardest part, and deliberately kept outside the effect system. Inside: one
reader thread splits the incoming stream into messages and sorts them into
"command replies" and "events"; one writer thread takes requests off a TBQueue
and sends them strictly one at a time (the protocol has no request ids, so
there is no other option), waits for a reply with a timeout, and honours a
minimum delay between sends; a watchdog thread sends heartbeats and reconnects
on a dropped link.
-}

{- The outward-facing record stays small:
data Connection = Connection
  { sendCmd :: Command -> IO (Either HeosError Response)
  , events :: TChan Event
  , cached :: TVar CacheState
  } -}
{-
For structured concurrency see the ki library: it guarantees that spawned
threads die together with their parent. That is exactly what daemons tend to
get wrong, and then leak.
-}
