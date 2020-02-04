{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies, PolyKinds, DataKinds, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.WordCount where

import qualified Data.ByteString as BS
import Data.Word

data Pair a b = Pair a b

data Statistics = Bytes | Words | Lines

newtype Tagged a = Tagged Int
  deriving (Show, Num)

data StatCompTyOf = Chunked | ByteOnly

data StatComputer st compTy where
  ChunkedComputer :: (st -> Word8 -> st)
                  -> (st -> BS.ByteString -> st)
                  -> StatComputer st 'Chunked
  ByteOnlyComputer :: (st -> Word8 -> st)
                   -> StatComputer st 'ByteOnly

class Statistic a where
  type ResultOf a = k | k -> a
  type StateOf a = k | k -> a
  type CompTyOf a :: StatCompTyOf
  initState :: StateOf a
  extractState :: StateOf a -> ResultOf a
  compute :: StatComputer (StateOf a) (CompTyOf a)

instance Statistic 'Bytes where
  type ResultOf 'Bytes = Tagged 'Bytes
  type StateOf 'Bytes = Tagged 'Bytes
  type CompTyOf 'Bytes = 'Chunked
  initState = 0
  extractState = id
  compute = ChunkedComputer (\st _ -> st + 1) (\st str -> st + Tagged (BS.length str))

instance Statistic 'Words where
  type ResultOf 'Words = Tagged 'Words
  type StateOf 'Words = Pair (Tagged 'Words) (Tagged 'Words)
  type CompTyOf 'Words = 'ByteOnly
  initState = Pair 0 0
  extractState (Pair ws wasSpace) = ws + 1 - wasSpace
  compute = ByteOnlyComputer step
    where
      step (Pair ws wasSpace) c = Pair (ws + (1 - wasSpace) * isSp) isSp
        where
          isSp | c == 32 || c - 9 <= 4 = 1
               | otherwise = 0

instance Statistic 'Lines where
  type ResultOf 'Lines = Tagged 'Lines
  type StateOf 'Lines = Tagged 'Lines
  type CompTyOf 'Lines = 'Chunked
  initState = 0
  extractState = id
  compute = ChunkedComputer (\st c -> st + if c == 10 then 1 else 0) (\st str -> st + Tagged (BS.count 10 str))

wc :: forall a. Statistic a => BS.ByteString -> ResultOf a
wc s = extractState $! runCompute compute
  where
    runCompute :: StatComputer (StateOf a) (CompTyOf a) -> StateOf a
    runCompute (ByteOnlyComputer step) = BS.foldl' step initState s
    runCompute (ChunkedComputer _ chunker) = chunker initState s
{-# SPECIALIZE wc :: BS.ByteString -> Tagged 'Words #-}

{-
wc :: BS.ByteString -> Tagged 'Words
wc s = extractState $! BS.foldl' step (initState :: Pair (Tagged 'Words) (Tagged 'Words)) s
  where
    step (Pair ws wasSpace) c = Pair (ws + (1 - wasSpace) * isSp) isSp
      where
        isSp | c == 32 || c - 9 <= 4 = 1
             | otherwise = 0
             -}
