{-# LANGUAGE Strict, RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies, FunctionalDependencies, PolyKinds, DataKinds, GADTs, TypeOperators, TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.WordCount where

import qualified Data.ByteString as BS
import Data.Proxy
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

class Statistic a res st comp | res -> a, st -> a, a -> res, a -> st, a -> comp where
  initState :: st
  extractState :: st -> res
  compute :: StatComputer st comp

instance Statistic 'Bytes (Tagged 'Bytes) (Tagged 'Bytes) 'Chunked where
  initState = 0
  extractState = id
  compute = ChunkedComputer (\st _ -> st + 1) (\st str -> st + Tagged (BS.length str))

data WordsState = WordsState { ws :: Int, wasSpace :: Int }

instance Statistic 'Words (Tagged 'Words) WordsState 'ByteOnly where
  initState = WordsState 0 1
  extractState WordsState { .. } = Tagged (ws + 1 - wasSpace)
  compute = ByteOnlyComputer step
    where
      step WordsState { .. } c = WordsState (ws + (1 - wasSpace) * isSp) isSp
        where
          isSp | c == 32 || c - 9 <= 4 = 1
               | otherwise = 0

instance Statistic 'Lines (Tagged 'Lines) (Tagged 'Lines) 'Chunked where
  initState = 0
  extractState = id
  compute = ChunkedComputer (\st c -> st + if c == 10 then 1 else 0) (\st str -> st + Tagged (BS.count 10 str))

instance (Statistic a resa sta compa, Statistic b resb stb compb, comp ~ CombineCompTy compa compb) => Statistic (a '::: b) (resa ::: resb) (sta ::: stb) comp where
  initState = initState ::: initState
  extractState (a ::: b) = extractState a ::: extractState b
  compute = case (compute :: StatComputer sta compa, compute :: StatComputer stb compb) of
                 (ByteOnlyComputer a, ChunkedComputer b _) -> ByteOnlyComputer $ combine a b
                 (ChunkedComputer a _, ByteOnlyComputer b) -> ByteOnlyComputer $ combine a b
                 (ByteOnlyComputer a, ByteOnlyComputer b)  -> ByteOnlyComputer $ combine a b
                 (ChunkedComputer stepA chunkA, ChunkedComputer stepB chunkB) -> ChunkedComputer (combine stepA stepB) (combine chunkA chunkB)
    where
      combine fa fb = \(a ::: b) w -> fa a w ::: fb b w

wc :: forall a res st comp. Statistic a res st comp => BS.ByteString -> res
wc s = extractState $! runCompute compute
  where
    runCompute :: StatComputer st comp -> st
    runCompute (ByteOnlyComputer step) = BS.foldl' step initState s
    runCompute (ChunkedComputer _ chunker) = chunker initState s

data SomeStats where
  MkSomeStats :: (Statistic s res st comp, Show res) => proxy s -> SomeStats

wc' :: SomeStats -> BS.ByteString -> String
wc' (MkSomeStats (_ :: proxy s)) str = show $ wc @s str
{-# INLINE wc' #-}

promoteStat :: Statistics -> SomeStats
promoteStat Bytes = MkSomeStats (Proxy :: Proxy 'Bytes)
promoteStat Words = MkSomeStats (Proxy :: Proxy 'Words)
promoteStat Lines = MkSomeStats (Proxy :: Proxy 'Lines)
{-# INLINE promoteStat #-}

promoteStats :: [Statistics] -> SomeStats
promoteStats [s] = promoteStat s
promoteStats (s:ss) = case (promoteStat s, promoteStats ss) of
                           (MkSomeStats (_ :: proxy1 s), MkSomeStats (_ :: proxy2 ss)) -> MkSomeStats (Proxy :: Proxy (s '::: ss))
