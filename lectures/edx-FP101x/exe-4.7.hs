module Main where

import Data.Char

let2int :: Char -> Int
let2int c
  | isLower c = ord c - ord 'a'
  | isUpper c = ord c - ord 'A'

int2let :: Int -> Char
int2let n
  | n < 65 = chr (ord 'a' + n)
  | otherwise = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c || isUpper c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
