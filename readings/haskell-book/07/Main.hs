module Main where

import Test.QuickCheck

main :: IO ()
main = quickCheck prop_tensDigit

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

tensDigit2 :: Integral a => a -> a
tensDigit2 x = d
  where (a, _) = x `divMod` 10
        (_, d) = a `divMod` 10

prop_tensDigit :: Integer -> Bool
prop_tensDigit x = tensDigit x == tensDigit2 x
