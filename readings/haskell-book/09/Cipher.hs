-- |
module Cipher where

import Data.Char
import Test.QuickCheck

cipher :: Int -> String -> String
cipher _ [] = []
cipher num (x : xs) = cChar num x : cipher num xs

cChar num x
  | isLower x = cLower num x
  | isUpper x = cUpper num x
  | otherwise = x

decipher :: Int -> String -> String
decipher _ [] = []
decipher num (x : xs) = dChar num x : decipher num xs

dChar num x
  | isLower x = dLower num x
  | isUpper x = dUpper num x
  | otherwise = x

cUpper num x = chr $ (ord x - ord 'A' + num) `mod` 26 + ord 'A'

cLower num x = chr $ (ord x - ord 'a' + num) `mod` 26 + ord 'a'

dUpper num x = chr $ (ord x - ord 'A' - num) `mod` 26 + ord 'A'

dLower num x = chr $ (ord x - ord 'a' - num) `mod` 26 + ord 'a'

main :: IO ()
main = (quickCheck . withMaxSuccess 10000) prop_cipher

genInt :: Gen Int
genInt = suchThat arbitrary (>= 0)

genMessage :: Gen String
genMessage = listOf1 $ elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ [' ', '\'', '.', ','])

prop_cipher :: Property
prop_cipher =
  forAll
    genInt
    (\n -> forAll genMessage (\m -> decipher n (cipher n m) == m))
