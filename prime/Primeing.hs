module Primeing where

sieve :: [Integer] -> [Integer]
sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p /= 0 ]

primes = sieve [2..]

primeFactors n = factor n primes
  where
    factor n (p:ps) 
      | p*p > n        = [n]
      | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
      | otherwise      = factor n ps
