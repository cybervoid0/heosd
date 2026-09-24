module Heos.Handler.Fake where

{- handler backed by recorded responses -}
{-
Recorded amplifier responses, no network at all. This is what you debug the
logic against at night without waking the house. Consider a third, recording
handler as well: it proxies to the real device and saves the exchange to disk,
so a day of real use turns into a set of test scenarios from life.
-}
