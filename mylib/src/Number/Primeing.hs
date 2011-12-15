module Number.Primeing 
       (primes,
        primeFactors)
       where

sieve :: (Integral a) => [a] -> [a] --[Integer] -> [Integer]
sieve []     = []
sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p /= 0 ]

primes :: (Integral a) => [a]
primes = sieve [2..]

primeFactors :: (Integral a) => a -> [a]
primeFactors n = factor n primes
  where
    factor n (p:ps) 
      | p*p > n        = [n]
      | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
      | otherwise      = factor n ps
