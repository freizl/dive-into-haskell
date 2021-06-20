-- |

module FibScanl where

import Test.QuickCheck

{-
scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q ls =
  q : (case ls of
         [] ->[]
         x:xs -> scanl f (f q x) xs)
-}

{-
  analysis how does this work (expands scanl at each step)
  amazed how it possibly work?!
fibs : 1 : scanl + 1 [1,??]  --> ?? becomes [1,??]
fibs = 1 : 1 : scanl + (+ 1 1) [1,??] --> ?? becomes [2,??]
fibs = 1 : 1 : 2 : scanl + (+ 2 1) [2,??]
fibs = 1 : 1 : 2 : 3 : scanl + (+ 3 2) [3,??]
fibs = 1 : 1 : 2 : 3 : 5 : scanl + (+ 5 3) [5,??]
-}
fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibs20 = take 20 fibs

fibsLessThan100 = takeWhile (< 100) fibs

factorial1 :: Word -> Word
factorial1 0 = 1
factorial1 n = n * factorial1 (n-1)

factorial2 :: Word -> Word
factorial2 0 = 1
factorial2 n = scanl (*) 1 [2..] !! (fromInteger . toInteger $ n - 1)

prop_factorial :: Word -> Bool
prop_factorial n = factorial1 n == factorial2 n

main :: IO ()
main = quickCheck prop_factorial
