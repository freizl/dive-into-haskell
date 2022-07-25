module Main where

import Test.QuickCheck

main :: IO ()
main = do
  quickCheck (withMaxSuccess 10000 prop_tensDigit)
  quickCheck (withMaxSuccess 10000 prop_hunsDigit)

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

hunsDigit :: Integral a => a -> a
hunsDigit x = d
  where xLast = x `div` 100
        d = xLast `mod` 10

hunsDigit2 :: Integral a => a -> a
hunsDigit2 x = d
  where (a, _) = x `divMod` 100
        (_, d) = a `divMod` 10

prop_hunsDigit :: Integer -> Bool
prop_hunsDigit x = hunsDigit x == hunsDigit2 x
