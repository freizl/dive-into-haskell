-- | 

module WordNumber where

import Data.List (intersperse)

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
  | otherwise = let (x, rem) = n `divMod` 10
                in
                  digits x ++ [rem]

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits

main = do
  -- "one-two-three-two-four-five-four-six"
  mapM_ (putStrLn . wordNumber) [ 12324546, 9, 10, 100, 2222]
