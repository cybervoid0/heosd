-- | Cutting the byte stream from a socket into lines. Pure: the reader
--   feeds it what recv returns and gets back the lines that are complete.
module Heos.Connection.Framing (
  lineEnd,
  splitLines,
  Pending (..),
  emptyPending,
  feedChunk,
) where

import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

-- | What ends a line on the wire.
lineEnd :: ByteString
lineEnd = "\r\n"

-- | Split a chunk of the socket stream into complete lines and the unfinished
--   tail. Lines are delimited by **\\r\\n**, which is dropped. The tail has no
--   delimiter yet: keep it and prepend it to the next chunk.
splitLines :: ByteString -> ([ByteString], ByteString)
splitLines bs
  | BS.null after = ([], line)
  -- add 'line' to the first element of the 'splitLines' function call result (a tuple)
  | otherwise = first (line :) (splitLines next)
 where
  (line, after) = BS.breakSubstring lineEnd bs
  next = BS.drop (BS.length lineEnd) after -- the stream after the delimiter

-- | The unfinished line between two recvs: the chunks since the last
--   'lineEnd', newest first, none of them containing it, and their total
--   length. A list rather than one ByteString, so a long line is copied
--   once when it completes, not on every chunk.
data Pending = Pending {chunks :: [ByteString], size :: Int} deriving (Show, Eq)

emptyPending :: Pending
emptyPending = Pending [] 0

-- | Feed one chunk: the lines it completes, in order, and the new tail.
--   'lineEnd' may straddle the boundary ("\\r" at the end of the tail,
--   "\\n" at the start of the chunk), so the search covers the tail's last
--   byte as well.
feedChunk :: Pending -> ByteString -> ([ByteString], Pending)
feedChunk pending chunk
  -- an empty chunk changes nothing; kept out of the list, or it would hide
  -- the last byte of the real tail from the straddle check
  | BS.null chunk = ([], pending)
  | lineEnd `BS.isInfixOf` joint = (complete, fromRest rest)
  | otherwise = ([], Pending (chunk : pending.chunks) (pending.size + BS.length chunk))
 where
  joint = case pending.chunks of
    c : _ -> BS.takeEnd 1 c <> chunk
    [] -> chunk
  (complete, rest) = splitLines (BS.concat (reverse (chunk : pending.chunks)))
  -- `rest` is a slice of the joined buffer and would keep all of it alive:
  -- copy it out, so the lines just delivered can be collected
  fromRest r
    | BS.null r = emptyPending
    | otherwise = Pending [BS.copy r] (BS.length r)
