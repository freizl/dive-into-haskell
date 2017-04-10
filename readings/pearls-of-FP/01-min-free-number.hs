module Main where

import Data.List

-- TODO: any chance to use quick-check
--
main :: IO ()
main = do
       print $ minfree sample1
       print $ minfree ([0..100] ++ [110..1000])
       print $ minfree ([0..100] ++ [110..1000]) == minfree2 ([0..100] ++ [110..1000])

minfree :: [Integer] -> Integer
minfree = minfrom 0

minfrom :: Integer -> [Integer] -> Integer
minfrom a [] = a
minfrom a xs = let n = toInteger (length xs)
                   b = a + 1 + (n`div`2)
                   (us, vs) = partition (< b) xs
                   m = toInteger (length us)
               in
                if m == b - a then minfrom b vs else minfrom a us

-- understand why check @len2 == b - a@
-- it bacially check for whether @us \\ [a..b]@ is empty or not.

minfree2 :: [Integer] -> Integer
minfree2 xs = minfrom2 0 (toInteger $ length xs, xs)

minfrom2 :: Integer -> (Integer, [Integer]) -> Integer
minfrom2 a (_, []) = a
minfrom2 a (n, xs) = let b = a + 1 + (n`div`2)
                         (us, vs) = partition (< b) xs
                         m = toInteger (length us)
                     in
                      if m == b - a then minfrom2 b (n-m, vs) else minfrom2 a (m, us)

sample1 :: [Integer]
sample1 = [08,23,09,0,12,11,10,13,07,41,04,14,21,05,17,03,19,02,06]
