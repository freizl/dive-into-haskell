module Main where

import Data.Char(toUpper, toLower)
import Data.List(sort,groupBy)

type WordCounter = (String, Int)

main :: IO ()
main = do
  str <- readFile "/home/haisgwu/Desktop/dvorak"
  putStrLn (show (length (words str)))
  putStrLn (show (sort ["the1", "the", "hello", "the"]))
  putStrLn (show (countWords ["the", "the", "the"]))
--  writeFile "output.txt" (map toUpper str)
--  putStrLn (show (words str))
  putStrLn (show (filter filter1 (collectWords (words str))))
  
filter1 :: WordCounter -> Bool
filter1 x = not ((length (fst x)) < 2 || (elem (fst x) ["the", "he", "and", "by", "for", "an"]))

collectWords :: [String] -> [WordCounter]
collectWords []    = []
collectWords words = map countWords (groupBy strEq (sort (map toLowerWord words)))

countWords :: [String] -> WordCounter
countWords [] = ("", 0)
countWords xs = ((head xs), (length xs))
                  
toLowerWord :: String -> String
toLowerWord x = (map toLower x)

strEq :: String -> String -> Bool
strEq a b = (map toUpper a) == (map toUpper b)
