module Main where

import Data.Word
{-- |
    Project euler 14
    Solution 1
--}
    
main :: IO ()
main = print $ p14

p14 = maximum [ (startChain n 0, n) | n <- [2..1000000] ]

-- ?? Word32 make faster
startChain :: Word32 -> Int -> Int
startChain 1 count    = count + 1
startChain n count    = startChain (intTransform n) (count+1)
                    
intTransform :: Word32 -> Word32
intTransform n
  | even n         = n `div` 2 
  | otherwise      = 3 * n + 1 


-- TODO: Try in functor/monad way
