{-# LANGUAGE Strict #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilyDependencies, PolyKinds, DataKinds #-}

module Data.WordCount where

import qualified Data.ByteString as BS
import Data.Word

data Pair a b = Pair a b

data Statistics = Bytes | Words | Lines

newtype Tagged a = Tagged Int
  deriving (Show, Num)

class Statistic a where
  type ResultOf a = k | k -> a
  type StateOf a = k | k -> a
  initState :: StateOf a
  extractState :: StateOf a -> ResultOf a
  step :: StateOf a -> Word8 -> StateOf a
  chunk :: StateOf a -> BS.ByteString -> StateOf a

instance Statistic 'Bytes where
  type ResultOf 'Bytes = Tagged 'Bytes
  type StateOf 'Bytes = Tagged 'Bytes
  initState = 0
  extractState = id
  step st _ = st + 1

instance Statistic 'Words where
  type ResultOf 'Words = Tagged 'Words
  type StateOf 'Words = Pair (Tagged 'Words) (Tagged 'Words)
  initState = Pair 0 0
  extractState (Pair ws wasSpace) = ws + 1 - wasSpace
  step (Pair ws wasSpace) c = Pair (ws + (1 - wasSpace) * isSp) isSp
    where
      isSp | c == 32 || c - 9 <= 4 = 1
           | otherwise = 0

instance Statistic 'Lines where
  type ResultOf 'Lines = Tagged 'Lines
  type StateOf 'Lines = Tagged 'Lines
  initState = 0
  extractState = id
  step st c = st + if c == 10 then 1 else 0

wc :: Statistic a => BS.ByteString -> ResultOf a
wc s = extractState $ BS.foldl' step initState s
{-# SPECIALIZE wc :: BS.ByteString -> Tagged 'Words #-}
