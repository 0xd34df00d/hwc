{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications, DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Strict #-}

module Main where

import qualified Data.ByteString.Lazy as BSL
import Control.Monad
import Options.Applicative
import System.Posix.Files
import System.IO.Posix.MMap

import Data.Dispatch
import Data.WordCount

data Options = Options
  { countBytes :: Bool
  , countChars :: Bool
  , countLines :: Bool
  , countMaxLineLength :: Bool
  , countWords :: Bool
  , files :: [FilePath]
  }

options :: Parser Options
options = Options
  <$> switch (long "bytes" <> short 'c' <> help "print the byte counts")
  <*> switch (long "chars" <> short 'm' <> help "print the character counts")
  <*> switch (long "lines" <> short 'l' <> help "print the newline counts")
  <*> switch (long "max-line-length" <> short 'L' <> help "print the maximum display width")
  <*> switch (long "words" <> short 'w' <> help "print the word counts")
  <*> some (argument str (metavar "FILES..."))

countMMapped, countReading :: [Statistics] -> FilePath -> IO ()
countMMapped stats path = do
  contents <- unsafeMMapFile path
  putStrLn $ $(dispatch 'wc 'contents) stats
countReading stats path = do
  contents <- BSL.readFile path
  putStrLn $ $(dispatch 'wcLazy 'contents) stats

main :: IO ()
main = do
  Options { .. } <- execParser $ info (options <**> helper) (fullDesc <> progDesc "Print newline, word, and byte counts for each file")
  let selectedStats = map snd $ filter fst [(countBytes, Bytes), (countChars, Chars), (countWords, Words), (countMaxLineLength, MaxLL), (countLines, Lines)]
  let stats | null selectedStats = [Bytes, Words, Lines]
            | otherwise = selectedStats
  forM_ files $ \path -> do
    stat <- getFileStatus path
    if isRegularFile stat || isSymbolicLink stat
      then countMMapped stats path
      else countReading stats path
