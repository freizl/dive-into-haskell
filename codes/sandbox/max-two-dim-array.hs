module Main where

import Test.QuickCheck

maximumArrayOfArray :: [[Int]] -> (Int, Int)
maximumArrayOfArray [] = (0, 0)
maximumArrayOfArray xss = (j, k)
                          where ((_, k), j) = maximumWithIndex $ map maximumWithIndex xss

maximumWithIndex :: Ord a =>
                    [a]
                    -> (a, Int) -- ^ (MaxValue, its Index)
maximumWithIndex xs = maximum (zip xs [0..n])
                      where n = length xs - 1

data1, data2 :: [[Int]]
data1 = [[1..3], [4..6], [7..9]]
data2 = [[2..4], [8, 9], [1..5]]


prop_test1, prop_test2 :: Bool
prop_test1 = maximumArrayOfArray data1 == (2, 2)
prop_test2 = maximumArrayOfArray data2 == (1, 1)

testSuits :: IO ()
testSuits = mapM_ quickCheck [prop_test1, prop_test2]
