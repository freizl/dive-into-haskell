module Main where

import System.IO(hSetEcho,stdin)

main :: IO ()
main = do
  putStrLn "Think of a word: "
  word <- sgetLine
  putStrLn "Try a guess"
  guess word
  
sgetLine :: IO String
sgetLine = do
  x <- getCh -- TODO: getCh
  if x == '\n' then
    do putChar x
       return []
    else
    do putChar '-'
       xs <- sgetLine
       return (x:xs)

guess :: String -> IO ()
guess word =
  do putStr "> "
     xs <- getLine
     if xs == word then
       do putStrLn "You got it"
       else
       do putStrLn ("The diff: <" ++ (worddiff xs word) ++ ">, try again")
          guess word

worddiff :: String -> String -> String
worddiff xs ys = [ if elem x ys then x else '-' | x <- xs]

getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c
