module Main where

import Data.List
import Primeing (primeFactors)

{-- |
    Project euler 12
--}
    
main :: IO ()
main = print $ problem_12

{-- |
    Solution 1
--}
p12 :: Int -> Int
p12 n = head $ filter (factorLimit n) [ smartGaus x | x <- [1..]]

smartGaus :: Int -> Int
smartGaus n
  | n <= 1     = n
  | n > 1      = (1+n)*n `div` 2

{-- |
  Is factor count under the limit  
--}
factorLimit :: Int -> Int -> Bool
factorLimit l n  
  | sqrtInt n < l   = False
  | otherwise         = length (factors  n) >= l


factors :: Int -> [Int]
factors n = concat [ [x, n `div` x] | x <- [1..sqrtInt n], n `mod` x == 0 ]

sqrtInt :: Int -> Int
sqrtInt = truncate . sqrt . fromIntegral

{-- | 
     Solution from haskell wiki
 --}
     
problem_12 = head $ filter ((> 500) . nDivisors) triangleNumbers          
  where nDivisors n = product $ map ((+1) . length) (group (primeFactors n))    
        triangleNumbers = scanl1 (+) [1..]
