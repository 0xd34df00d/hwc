{-# LANGUAGE TypeApplications, DataKinds #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BS
import System.Environment
import System.IO.Posix.MMap

import Data.WordCount

main :: IO ()
main = do
  [path] <- getArgs
  contents <- unsafeMMapFile path
  print $ wc @'Words contents
