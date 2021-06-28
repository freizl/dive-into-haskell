module Main where

import Data.Foldable
import Data.Monoid

main :: IO ()
main = putStrLn "Hello, Haskell!"

s1 = foldr (++) "" ["hello", " haskell"]
s2 = fold ["hello", " haskell", "!!"]
s3 = concat ["hello", " haskell", "!!"]

t1 = fmap length Just 2
