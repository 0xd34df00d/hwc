{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeApplications #-}

import qualified Data.ByteString.Char8 as BS
import Data.List
import Test.Hspec
import Test.QuickCheck

import Data.WordCount

main :: IO ()
main = hspec $
  describe "Some QC properties" $ do
    it "Counts bytes correctly" $ property $
      \(getASCIIString -> str) -> wc @'Bytes (BS.pack str) `shouldBe` genericLength str
    it "Counts words correctly" $ property $
      \(getASCIIString -> str) -> wc @'Words (BS.pack str) `shouldBe` genericLength (words str)
    it "Counts lines correctly" $ property $
      \(getASCIIString -> str) -> wc @'Lines (BS.pack str) `shouldBe` genericLength (filter (== '\n') str)
