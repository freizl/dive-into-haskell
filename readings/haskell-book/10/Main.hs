module Main where

import qualified DBProcess
import qualified FibScanl
import GHC.Float
import Test.QuickCheck

main :: IO ()
main = do
  (quickCheck . withMaxSuccess 10000) prop_seekritFunc
  (quickCheck . withMaxSuccess 10000) prop_or
  (quickCheck . withMaxSuccess 10000) prop_any
  (quickCheck . withMaxSuccess 10000) prop_elem
  (quickCheck . withMaxSuccess 10000) prop_elem2
  (quickCheck . withMaxSuccess 10000) prop_reverse
  (quickCheck . withMaxSuccess 10000) prop_map
  (quickCheck . withMaxSuccess 10000) prop_filter
  (quickCheck . withMaxSuccess 10000) prop_squish

seekritFunc :: String -> Int
seekritFunc x =
  let ws = words x
      lx = length ws
   in if lx == 0 then 0 else div (sum (map length ws)) lx

seekritFunc2 :: String -> Float
seekritFunc2 [] = 0
seekritFunc2 x = if s2 == 0 then 0 else int2Float s1 / int2Float s2
  where
    s1 = sum (map length (words x))
    s2 = length (words x)

-- | meaningless testing (floor the result) but just have fun with quickcheck
prop_seekritFunc :: String -> Bool
prop_seekritFunc str = seekritFunc str == floor (seekritFunc2 str)

myOr :: [Bool] -> Bool
myOr = foldr (||) False
prop_or :: [Bool] -> Bool
prop_or xs = myOr xs == or xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> f a || b) False
prop_any :: [Integer] -> Bool
prop_any xs = myAny even xs == any even xs

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a b -> a == x || b) False
prop_elem :: Integer -> [Integer] -> Bool
prop_elem x xs = myElem x xs == elem x xs
prop_elem2 :: Char -> [Char] -> Bool
prop_elem2 x xs = myElem x xs == elem x xs

myReverse :: [a] -> [a]
myReverse = foldr (\a b -> b ++ [a]) []
prop_reverse :: [Integer] -> Bool
prop_reverse xs = myReverse xs == reverse xs

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []
prop_map :: [Integer] -> Bool
prop_map xs =
  let f = (+ 2)
   in myMap f xs == map f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a:b else b) []
prop_filter :: [Integer] -> Bool
prop_filter xs = myFilter even xs == filter even xs

squish :: [[a]] -> [a]
squish = foldr (++) []
prop_squish :: [[Integer]] -> Bool
prop_squish xss = squish xss == concat xss
