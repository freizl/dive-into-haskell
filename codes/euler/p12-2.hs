module Main where

import Data.List
import Number.Primeing (primeFactors)

{-- |
    Project euler 12
    find detail fo Number.Primeing at ../mylib directory
--}
    
main :: IO ()
main = print $ problem_12

{-- | 
     Solution from haskell wiki
 --}
     
problem_12 = head $ filter ((> 500) . nDivisors) triangleNumbers          
  where nDivisors n = product $ map ((+1) . length) (group (primeFactors n))    
        triangleNumbers = scanl1 (+) [1..]
