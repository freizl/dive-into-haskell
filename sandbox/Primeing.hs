module Main where

import Test.QuickCheck

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p /= 0 ]

primes = sieve [2..]
