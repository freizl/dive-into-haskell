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

cl ::
  -- | ( message char, seed char )
  (Char, Char) ->
  Char
cl (a, b)
  | isUpper a = chr $ ord 'A' + (ord a - ord 'A' + ord b - ord 'A') `mod` 26
  | isLower a = chr $ ord 'a' + (ord a - ord 'a' + ord b - ord 'a') `mod` 26
  | otherwise = a

decipher :: Seed -> Message -> Message
decipher s m = map dl (genLetterPair s m)

dl :: (Char, Char) -- ^ (message char, seed char)
  -> Char
dl (a, b)
  | isUpper a = chr $ ord 'A' + (ord a - ord 'A' - (ord b - ord 'A')) `mod` 26
  | isLower a = chr $ ord 'a' + (ord a - ord 'a' - (ord b - ord 'a')) `mod` 26
  | otherwise = a

main :: IO ()
main = quickCheck prop_cipher

-- | TODO : input seed shall all be letters
--   probably has to declare data type and implement type class Arbitrary
--
prop_cipher :: String -- ^ Seed
  -> String -- ^ Message
  -> Bool
prop_cipher s m = decipher s (cipher s m) == m
