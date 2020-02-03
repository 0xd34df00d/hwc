{-# LANGUAGE Strict #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.WordCount where

import qualified Data.ByteString as BS

data State = State
  { bs :: Int
  , ws :: Int
  , ls :: Int
  , wasSpace :: Int
  }

wc :: BS.ByteString -> (Int, Int, Int)
wc s = (bs, ws + 1 - wasSpace, ls)
  where
    State { .. } = BS.foldl' go (State 0 0 0 1) s

    go State { .. } c = State (bs + 1) (ws + addWord) (ls + addLine) isSp
      where
        isSp | c == 32 || c - 9 <= 4 = 1
             | otherwise = 0
        addLine | c == 10 = 1
                | otherwise = 0
        addWord = (1 - wasSpace) * isSp
{-# INLINE wc #-}