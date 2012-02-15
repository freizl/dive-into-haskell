module Main where

import Data.List

{- 
  Chapter 11 code toy
-}

main = print $ head $ solutions [1,3,7,10,25,50] 765

data Op = Add | Sub | Mul | Div
          deriving (Show)

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

data Expr = Val Int | App Op Expr Expr
instance Show Expr where                      
  show (Val n) = show n
  show (App Add l r) = "(" ++ show l ++ "+" ++ show r ++ ")"
  show (App Sub l r) = "(" ++ show l ++ "-" ++ show r ++ ")"
  show (App Mul l r) = "(" ++ show l ++ "*" ++ show r ++ ")"  
  show (App Div l r) = "(" ++ show l ++ "/" ++ show r ++ ")"  

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l
                                , y <- eval r
                                , valid o x y]

choices :: [a] -> [[a]]
choices = concat . map permutations . subsequences

-- | safe check that all value in one Expr must be one of choices.
-- 
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns)
                  && eval e == [n]

split :: [a] -> [([a], [a])]
split []                      =  []
split [_]                     =  []
split (x:xs)                  =  ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns
              , l        <- exprs ls
              , r        <- exprs rs
              , e        <- combine l r]

-- | FIXME: enum the OP type
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add, Sub, Mul, Div]]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns
                    , e   <- exprs ns'
                    , eval e == [n] ]
