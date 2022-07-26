-- | 

module Recursion where

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)
{-
dividedBy 15 2
= go 13 2 1
= go 11 2 2
= go 9 2 3
= go 7 2 4
= go 5 2 5
= go 3 2 6
= go 1 2 7
= (7, 1)
-}

-- Assume n >= 0
--
mySum :: (Eq a, Num a) => a -> a
mySum n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = n + mySum (n-1)


myMul :: Integral a => a -> a -> a
myMul x y
  | x > y            = myMul y x
  | toInteger x == 0 = 0
  | toInteger x == 1 = y
  | otherwise        = y + myMul (x-1) y
