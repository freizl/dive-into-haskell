-- |

module Main where

import Data.Monoid
import First
import Trivial

main :: IO ()
main = do
  print "hello chapter 16"
  -- print (f1 `mempty` f2 $ ( Sum 8 :: Sum Int))

f1 :: Sum Int -> Sum Int
f1 x = x + 2

f2 :: Sum Int -> Sum Int
f2 y = y + 3
