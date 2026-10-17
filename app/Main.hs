module Main (main) where

import Heos.FirstTalk (firstTalk)

import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  firstTalk