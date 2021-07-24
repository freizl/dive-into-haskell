module Main where

import Queue

main :: IO ()
main = do
  let q1 = def
  let q2 = push 1 q1
  putStrLn "Hello, Haskell!"
