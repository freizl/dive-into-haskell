-- | TODO:
-- 1. How to handle negative number?
-- 2. How to fix incomplete patterns for `digitToWord`?
--

module WordNumber where

import Test.Hspec
import Data.List

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits n
  | n < 10 = [n]
  | otherwise = digits (n `div` 10) ++ [n `mod` 10]

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"
  describe "digits" $ do
    it "returns [0] for 0" $ do
      digits 0 `shouldBe` [0]
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1,0,0]
    it "returns [2, 1, 3] for 213" $ do
      digits 213 `shouldBe` [2,1,3]
    it "returns [1, 3] for 013" $ do
      digits 013 `shouldBe` [1,3]
  describe "wordNumber" $ do
    it "one-zero-zero for 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"
