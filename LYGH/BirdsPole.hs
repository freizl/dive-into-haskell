module BirdsPole where

-- http://learnyouahaskell.com/a-fistful-of-monads

type Birds = Int
data Pole  = P {left::Birds, right::Birds} deriving (Show)

landLeft :: Birds -> Pole -> [Pole]
landLeft b (P l r) 
    | abs ((l+b) - r) < 4  = [P (l+b) r]
    | otherwise            = []
 
landRight :: Birds -> Pole -> [Pole]
landRight b (P l r) 
    | abs ((r+b) - l) < 4  = [P l (r+b)]
    | otherwise            = []
