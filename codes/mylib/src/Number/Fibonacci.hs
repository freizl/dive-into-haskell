module Number.Fibonacci where

-- |
-- FROM: http://www.haskell.org/haskellwiki/The_Fibonacci_sequence

import Data.List

-- | O(n) solution

fib1 n = fibs !! n
fibs   = 0 : 1 : zipWith (+) fibs (tail fibs)


-- | matrix solution
fib2 n = head (apply (Matrix [[0,1], [1,1]] ^ n) [0,1])

newtype Matrix a = Matrix [[a]] deriving (Eq, Show)
instance Num a => Num (Matrix a) where
    Matrix as + Matrix bs = Matrix (zipWith (zipWith (+)) as bs)
    Matrix as - Matrix bs = Matrix (zipWith (zipWith (-)) as bs)
    Matrix as * Matrix bs =
       Matrix [[sum $ zipWith (*) a b | b <- transpose bs] | a <- as]
    negate (Matrix as) = Matrix (map (map negate) as)
    fromInteger x = Matrix (iterate (0:) (fromInteger x : repeat 0))
    abs m = m
    signum _ = 1


apply :: Num a => Matrix a -> [a] -> [a]
apply (Matrix as) b = [sum (zipWith (*) a b) | a <- as]
