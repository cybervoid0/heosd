module Heos.Time (Seconds (..), toMicros) where

newtype Seconds = Seconds Double deriving (Show, Eq)

toMicros :: Seconds -> Int
toMicros (Seconds s) = round (s * 1000000)