{-# LANGUAGE Strict #-}
{-# LANGUAGE RecordWildCards #-}

module Data.WordCount where

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char

data State = State
  { cs :: Int
  , ws :: Int
  , ls :: Int
  , wasSpace :: Int
  }

wc :: BS.ByteString -> (Int, Int, Int)
wc s = (cs, ws, ls)
  where
    State { .. } = BS.foldl' go (State 0 0 0 0) s

    go State { .. } c = State (cs + 1) (ws + addWord) (ls + addLine) isSp
      where
        isSp | isSpace c = 1
             | otherwise = 0
        addLine | c == '\n' = 1
                | otherwise = 0
        addWord = (1 - wasSpace) * isSp
{-# INLINE wc #-}