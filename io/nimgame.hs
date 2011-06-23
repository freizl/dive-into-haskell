module Main where

import Control.Monad

type Board = [Int]

main :: IO ()
main = do showBoard (reverse [1..5])

showBoard :: Board -> IO ()
showBoard b = forM_ (map wrapRow b) (\x -> putStrLn x)
                 
wrapRow :: Int -> String
wrapRow n = [ '*' | x <- [1..n] ]
