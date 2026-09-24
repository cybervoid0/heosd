module Heos.Service where

{- scenarios: find-and-play, status, volume -}
{-
"Find and play" is not a single protocol command but a sequence: list the
sources, search the relevant ones, pick the best match, enqueue it, start
playback. The "best match" logic is exactly the part worth debugging without
the amplifier in the loop.
-}
