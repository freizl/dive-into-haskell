-- |
module VigenereCiper where

import Data.Char
import Test.QuickCheck

type Seed = String

type Message = String

genLetterPair ::
  Seed ->
  Message ->
  [(Char, Char)]
genLetterPair s str = go str ciphers
  where
    go [] _ = []
    go _ [] = []
    go (x : xs) b@(y : ys) =
      if isLetter x
        then (x, y) : go xs ys
        else (x, x) : go xs b
    ciphers = concat (repeat s)

cipher :: Seed -> Message -> Message
cipher s m = map cl (genLetterPair s m)

-- | `isLower '\353' return true. Why??`
-- `\353` is decimal and is `\x161` in hex,
-- which represents a latin character (unicode)
-- see more details at https://www.compart.com/en/unicode/U+0161
-- So apparently, `isLower` works not only for alpha char but also unicode char
--
cl ::
  -- | ( message char, seed char )
  (Char, Char) ->
  Char
cl (a, b)
  | ord a >= ord 'A' && ord a <= ord 'Z' = chr $ ord 'A' + (ord a - ord 'A' + ord b - ord 'A') `mod` 26
  | ord a >= ord 'a' && ord a <= ord 'z' = chr $ ord 'a' + (ord a - ord 'a' + ord b - ord 'a') `mod` 26
  | otherwise = a

decipher :: Seed -> Message -> Message
decipher s m = map dl (genLetterPair s m)

dl ::
  -- | (message char, seed char)
  (Char, Char) ->
  Char
dl (a, b)
  | ord a >= ord 'A' && ord a <= ord 'Z' = chr $ ord 'A' + (ord a - ord 'A' - (ord b - ord 'A')) `mod` 26
  | ord a >= ord 'a' && ord a <= ord 'z' = chr $ ord 'a' + (ord a - ord 'a' - (ord b - ord 'a')) `mod` 26
  | otherwise = a

main :: IO ()
main = (quickCheck . withMaxSuccess 10000) prop_cipher

genSeed :: Gen String
genSeed = listOf1 $ elements ['A' .. 'Z']

prop_cipher :: Property
prop_cipher =
  forAll genSeed (\s -> forAll arbitrary (\m -> decipher s (cipher s m) == m))
