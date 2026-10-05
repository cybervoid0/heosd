module Heos.Connection.Internal where

import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

-- | Split a chunk of the socket stream into complete lines and the unfinished
--   tail. Lines are delimited by **\\r\\n**, which is dropped. The tail has no
--   delimiter yet: keep it and prepend it to the next chunk.
splitLines :: ByteString -> ([ByteString], ByteString)
splitLines bs
  | BS.null after = ([], line)
  -- add 'line' to the first element of the 'splitLines' function call result (a tuple)
  | otherwise = first (line :) (splitLines next)
 where
  delimiter = "\r\n"
  (line, after) = BS.breakSubstring delimiter bs
  next = BS.drop (BS.length delimiter) after -- the stream after the delimiter
