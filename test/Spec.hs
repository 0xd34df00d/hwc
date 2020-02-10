{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeApplications #-}

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.List
import Test.Hspec
import Test.QuickCheck

import Data.WordCount

wrapUnicode :: UnicodeString -> (BS.ByteString, T.Text)
wrapUnicode ustr = (T.encodeUtf8 txt, txt)
  where
    txt = T.pack $ getUnicodeString ustr

main :: IO ()
main = hspec $ do
  describe "ASCII support" $ do
    it "Counts bytes correctly" $ property $
      \(getASCIIString -> str) -> wc @'Bytes (BS.pack str) `shouldBe` genericLength str
    it "Counts words correctly" $ property $
      \(getASCIIString -> str) -> wc @'Words (BS.pack str) `shouldBe` genericLength (words str)
    it "Counts lines correctly" $ property $
      \(getASCIIString -> str) -> wc @'Lines (BS.pack str) `shouldBe` genericLength (filter (== '\n') str)
  describe "UTF8 support" $ do
    it "Counts bytes correctly" $ property $
      \(wrapUnicode -> (bs, _))   -> wc @'Bytes bs `shouldBe` fromIntegral (BS.length bs)
    it "Counts words correctly" $ property $
      \(wrapUnicode -> (bs, txt)) -> wc @'Words bs `shouldBe` genericLength (T.words txt)
    it "Counts lines correctly" $ property $
      \(wrapUnicode -> (bs, txt)) -> wc @'Lines bs `shouldBe` fromIntegral (T.count "\n" txt)
