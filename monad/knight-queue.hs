{-
SOURCE: http://learnyouahaskell.com/a-fistful-of-monads#the-list-monad

1. As an exercise, you can change this function 
   so that when you can reach one position from the other, 
   it tells you which moves to take.
2.  how to modify this function so that 
    we also pass it the number of moves to take instead of 
    that number being hardcoded.
-}

module Main where

import Control.Monad (foldM)

main = putStrLn "Hello World"

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = filter onBoard
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
    ]
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]

in3 :: KnightPos -> [KnightPos]
in3 start = moveKnight start >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

type Count = Int
-- | do not hard code move steps
--   Any better impl??
increaseN :: KnightPos -> Count -> [KnightPos]
increaseN start n = foldM (\ x y -> y x) start (replicate n moveKnight)

inx :: Int -> KnightPos -> [KnightPos]
inx start n = foldM (\a _ -> moveKnight a) start [1..n]

-- | Understand List Monad
in3' :: KnightPos -> [[KnightPos]]
in3' start = do
	first  <- moveKnight start
	second <- moveKnight first
	third  <- moveKnight second
	return [start, first, second, third]

-- | show all routes when successful
canReachIn3' :: KnightPos -> KnightPos -> [[KnightPos]]
canReachIn3' start end =  filter (\x -> end == last x) (in3' start)
	                       
