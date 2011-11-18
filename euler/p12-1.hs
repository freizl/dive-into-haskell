module Main where

import Data.List
import Primeing (primeFactors)

{-- |
    Project euler 12
    Solution 1
--}
    
main :: IO ()
main = print $ p12 500

p12 :: Int -> Int
p12 n = head $ filter (factorLimit n) [ smartGaus x | x <- [1..]]

smartGaus :: Int -> Int
smartGaus n = (1+n)*n `div` 2

{-- |
  Is factor count under the limit  
--}
factorLimit :: Int -> Int -> Bool
factorLimit l n  
  | 2 * sqrtInt n < l    = False
  | otherwise            = length (factors  n) >= l


factors :: Int -> [Int]
factors n = concat [ [x, n `div` x] | x <- [1..sqrtInt n], n `mod` x == 0 ]

sqrtInt :: Int -> Int
sqrtInt = truncate . sqrt . fromIntegral
