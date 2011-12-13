module Number.Primeing where

sieve :: [Integer] -> [Integer]
sieve []     = []
sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p /= 0 ]

primes :: [Integer]
primes = sieve [2..]

primeFactors :: Integer -> [Integer]
primeFactors n = factor n primes
  where
    factor n (p:ps) 
      | p*p > n        = [n]
      | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
      | otherwise      = factor n ps
