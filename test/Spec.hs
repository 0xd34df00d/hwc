{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeApplications #-}

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Char
import Data.List
import Test.Hspec
import Test.Hspec.Core.QuickCheck
import Test.QuickCheck

import Data.WordCount

wrapUnicode :: UnicodeString -> (BS.ByteString, T.Text)
wrapUnicode ustr = (T.encodeUtf8 txt, txt)
  where
    txt = T.pack $ getUnicodeString ustr

replaceNonAsciiSpaces :: Char -> Char
replaceNonAsciiSpaces ch | ch >= chr 127 && isSpace ch = '_'
                         | otherwise = ch

main :: IO ()
main = hspec $ parallel $ modifyMaxSuccess (const 10000) $ modifyMaxSize (const 1000) $ do
  describe "ASCII support" $ do
    it "Counts bytes correctly" $ property $
      \(getASCIIString -> str) -> wc @'Bytes (BS.pack str) `shouldBe` genericLength str
    it "Counts chars correctly" $ property $
      \(getASCIIString -> str) -> wc @'Chars (BS.pack str) `shouldBe` genericLength str
    it "Counts words correctly" $ property $
      \(getASCIIString -> str) -> wc @'Words (BS.pack str) `shouldBe` genericLength (words str)
    it "Counts lines correctly" $ property $
      \(getASCIIString -> str) -> wc @'Lines (BS.pack str) `shouldBe` genericLength (filter (== '\n') str)
  describe "UTF8 support" $ do
    it "Counts bytes correctly" $ property $
      \(wrapUnicode -> (bs, _))   -> wc @'Bytes bs `shouldBe` fromIntegral (BS.length bs)
    it "Counts chars correctly" $ property $
      \(wrapUnicode -> (bs, txt)) -> wc @'Chars bs `shouldBe` fromIntegral (T.length txt)
    it "Counts words correctly" $ property $
      \(wrapUnicode -> (bs, txt)) -> wc @'Words bs `shouldBe` genericLength (T.words $ T.map replaceNonAsciiSpaces txt)
    it "Counts lines correctly" $ property $
      \(wrapUnicode -> (bs, txt)) -> wc @'Lines bs `shouldBe` fromIntegral (T.count "\n" txt)
