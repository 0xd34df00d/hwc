{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

import qualified Data.ByteString.Char8 as BS
import Test.Hspec
import Test.QuickCheck

import Data.WordCount

main :: IO ()
main = hspec $
  describe "Some QC properties" $
    it "Counts ASCII things correctly" $ property $
      \(getASCIIString -> str) -> wc (BS.pack str) `shouldBe` (length str, length $ words str, length $ filter (== '\n') str)
