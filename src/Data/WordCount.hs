{-# LANGUAGE Strict, RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies, PolyKinds, DataKinds, GADTs, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.WordCount where

import qualified Data.ByteString as BS
import Data.Word

infixr 5 :::
data a ::: b = a ::: b deriving (Show)

data Statistics = Bytes | Words | Lines

newtype Tagged a = Tagged Int deriving (Show, Num)

data StatCompTyOf = Chunked | ByteOnly

type family CombineCompTy a b where
  CombineCompTy 'Chunked 'Chunked = 'Chunked
  CombineCompTy _ _ = 'ByteOnly

data StatComputer st compTy where
  ChunkedComputer :: (st -> Word8 -> st)
                  -> (st -> BS.ByteString -> st)
                  -> StatComputer st 'Chunked
  ByteOnlyComputer :: (st -> Word8 -> st)
                   -> StatComputer st 'ByteOnly

type StatComputerOf a = StatComputer (StateOf a) (CompTyOf a)

class Statistic a where
  type ResultOf a = k | k -> a
  type StateOf a = k | k -> a
  type CompTyOf a :: StatCompTyOf
  initState :: StateOf a
  extractState :: StateOf a -> ResultOf a
  compute :: StatComputerOf a

instance Statistic 'Bytes where
  type ResultOf 'Bytes = Tagged 'Bytes
  type StateOf 'Bytes = Tagged 'Bytes
  type CompTyOf 'Bytes = 'Chunked
  initState = 0
  extractState = id
  compute = ChunkedComputer (\st _ -> st + 1) (\st str -> st + Tagged (BS.length str))

data WordsState = WordsState { ws :: Int, wasSpace :: Int }

instance Statistic 'Words where
  type ResultOf 'Words = Tagged 'Words
  type StateOf 'Words = WordsState
  type CompTyOf 'Words = 'ByteOnly
  initState = WordsState 0 0
  extractState WordsState { .. } = Tagged (ws + 1 - wasSpace)
  compute = ByteOnlyComputer step
    where
      step WordsState { .. } c = WordsState (ws + (1 - wasSpace) * isSp) isSp
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

instance (Statistic a, Statistic b) => Statistic (a '::: b) where
  type ResultOf (a '::: b) = ResultOf a ::: ResultOf b
  type StateOf (a '::: b) = StateOf a ::: StateOf b
  type CompTyOf (a '::: b) = CombineCompTy (CompTyOf a) (CompTyOf b)
  initState = initState ::: initState
  extractState (a ::: b) = extractState a ::: extractState b
  compute = case (compute :: StatComputerOf a, compute :: StatComputerOf b) of
                 (ByteOnlyComputer a, ChunkedComputer b _) -> ByteOnlyComputer $ combine a b
                 (ChunkedComputer a _, ByteOnlyComputer b) -> ByteOnlyComputer $ combine a b
                 (ByteOnlyComputer a, ByteOnlyComputer b)  -> ByteOnlyComputer $ combine a b
                 (ChunkedComputer stepA chunkA, ChunkedComputer stepB chunkB) -> ChunkedComputer (combine stepA stepB) (combine chunkA chunkB)
    where
      combine fa fb = \(a ::: b) w -> fa a w ::: fb b w

wc :: forall a. Statistic a => BS.ByteString -> ResultOf a
wc s = extractState $! runCompute compute
  where
    runCompute :: StatComputerOf a -> StateOf a
    runCompute (ByteOnlyComputer step) = BS.foldl' step initState s
    runCompute (ChunkedComputer _ chunker) = chunker initState s
