{-# LANGUAGE Strict, RecordWildCards, BinaryLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies, FunctionalDependencies, PolyKinds, DataKinds, GADTs, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.WordCount where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Bits
import Data.Word

data Statistics = Bytes | Chars | Words | MaxLL | Lines deriving (Eq, Ord)
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
  prettyPrint :: res -> String
  compute :: StatComputer st comp

newtype Tagged a = Tagged Word64 deriving (Eq, Show, Num)

instance Statistic 'Bytes (Tagged 'Bytes) (Tagged 'Bytes) 'Chunked where
  initState = 0
  extractState = id
  prettyPrint (Tagged n) = show n <> " bytes"
  compute = ChunkedComputer (\st _ -> st + 1) (\st str -> st + fromIntegral (BS.length str))

instance Statistic 'Chars (Tagged 'Chars) (Tagged 'Chars) 'ByteOnly where
  initState = 0
  extractState = id
  prettyPrint (Tagged n) = show n <> " characters"
  compute = ByteOnlyComputer $ \cnt c -> cnt + 1 - fromIntegral (   ((c .&. 0b10000000) `shiftR` 7)
                                                                .&. (1 - ((c .&. 0b01000000) `shiftR` 6))
                                                                )

data WordsState = WordsState { ws :: Word64, wasSpace :: Word64 }

instance Statistic 'Words (Tagged 'Words) WordsState 'ByteOnly where
  initState = WordsState 0 1
  extractState WordsState { .. } = Tagged (ws + 1 - wasSpace)
  prettyPrint (Tagged n) = show n <> " words"
  compute = ByteOnlyComputer step
    where
      step WordsState { .. } c = WordsState (ws + (1 - wasSpace) * isSp) isSp
        where
          isSp | c == 32 || c - 9 <= 4 = 1
               | otherwise = 0

data MaxLLState = MaxLLState { maxLen :: Word64, curLen :: Word64 }

instance Statistic 'MaxLL (Tagged 'MaxLL) MaxLLState 'ByteOnly where
  initState = MaxLLState 0 0
  extractState MaxLLState { .. } = Tagged $ max maxLen curLen
  prettyPrint (Tagged n) = show n <> " max line length"
  compute = ByteOnlyComputer step
    where
      step MaxLLState { .. } 9 = MaxLLState maxLen $ curLen + 8 - (curLen `rem` 8)
      step MaxLLState { .. } 8 = MaxLLState maxLen $ max 0 (curLen - 1)
      step MaxLLState { .. } c | c == 10
                              || c == 12
                              || c == 13 = MaxLLState (max maxLen curLen) 0
                               | c < 32 = MaxLLState maxLen curLen
      step MaxLLState { .. } _ = MaxLLState maxLen (curLen + 1)

instance Statistic 'Lines (Tagged 'Lines) (Tagged 'Lines) 'Chunked where
  initState = 0
  extractState = id
  prettyPrint (Tagged n) = show n <> " lines"
  compute = ChunkedComputer (\st c -> st + if c == 10 then 1 else 0) (\st str -> st + fromIntegral (BS.count 10 str))

infixr 5 :::
data a ::: b = a ::: b deriving (Show)

instance (Statistic a resa sta compa, Statistic b resb stb compb, comp ~ CombineCompTy compa compb)
       => Statistic (a '::: b) (resa ::: resb) (sta ::: stb) comp where
  initState = initState ::: initState
  extractState (a ::: b) = extractState a ::: extractState b
  prettyPrint (a ::: b) = prettyPrint a <> "\n" <> prettyPrint b
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

wcLazy :: forall a res st comp. Statistic a res st comp => BSL.ByteString -> res
wcLazy s = extractState $! runCompute compute
  where
    runCompute :: StatComputer st comp -> st
    runCompute (ByteOnlyComputer step) = BSL.foldl' step initState s
    runCompute (ChunkedComputer _ chunker) = BSL.foldlChunks chunker initState s
