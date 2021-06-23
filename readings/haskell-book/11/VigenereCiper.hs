-- |
module VigenereCiper where

import Data.Char

genLetterPair ::
  -- | seed
  String ->
  -- | target message
  String ->
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
