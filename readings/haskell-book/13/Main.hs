module Main where

import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Please input your name: "
  name <- getLine
  sayHello name
  dogs

sayHello :: [Char] -> IO ()
sayHello name = putStrLn ("Hi " ++ name ++ "!")

dogs :: IO ()
dogs = return ()
