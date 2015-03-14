{-# LANGUAGE MonadComprehensions #-}

module Main where

import Data.Monoid
import Data.Maybe

test :: Int -> String -> Int -> Maybe String
test x str n = listToMaybe [ str | n `mod` x == 0 ]

fizz :: Int -> Maybe String
fizz = test 3 "fizz"

buzz :: Int -> Maybe String
buzz = test 5 "buzz"

fz :: Int -> String
fz n =  fromMaybe (show n) $ mconcat $ [fizz n, buzz n]


fz2 :: Int -> String
fz2 i = fromMaybe (show i) $ mappend ["fizz" | i `rem` 3 == 0]
                                     ["buzz" | i `rem` 5 == 0]

main :: IO ()
main = mapM_ print [(x, fz x, fzhh x) | x <- [1..20] ++ [70..80] ++ [1150..1160]]

hiss, howl :: Int -> Maybe String
hiss = test 7 "hiss"
howl = test 11 "howl"

fzhh :: Int -> String
fzhh n = fromMaybe (show n) $ mconcat $ [fizz n, buzz n, hiss n, howl n]
