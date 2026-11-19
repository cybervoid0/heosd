module Main (main) where

import Remote (remote)

import Settings (loadParams)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  params <- loadParams
  remote params