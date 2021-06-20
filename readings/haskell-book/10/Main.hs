module Main where

import qualified DBProcess
import qualified FibScanl
import GHC.Float
import Test.QuickCheck

main :: IO ()
main = do
  (quickCheck . withMaxSuccess 2000) prop_seekritFunc
  (quickCheck . withMaxSuccess 2000) prop_or

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

myOr = foldr (||) False

prop_or :: [Bool] -> Bool
prop_or xs = myOr xs == or xs
