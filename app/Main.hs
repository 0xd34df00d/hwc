module Main where

import qualified Data.ByteString.Lazy.Char8 as BS
import System.Environment

import Data.WordCount

main :: IO ()
main = do
  [path] <- getArgs
  contents <- BS.readFile path
  print $ wc contents
