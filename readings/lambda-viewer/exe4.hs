module Main where

import Data.Char
import Types
import Test.QuickCheck

main :: IO ()
main = test

{- Exercise 4 -}

char :: (Char -> Bool) -> ReadS Char
char f (c:s) | f c = [(c, s)]
char f _           = []

variable :: ReadS Var
variable s = [ (V [c], s1) | (c, s1) <- char isAlpha s ]

variable2 :: ReadS Var
variable2 s = [ (V [c1, c2], s2) | (c1, s1) <- char isAlpha s, 
                                   (c2, s2) <- char isAlpha s1 ]
variableN :: Int -> ReadS Var
variableN n s 
  | length s < n = [] 
  | otherwise    = foldl f init [ y | x <- take n s, y <- [variable [x]]]
                   where init = [(V "", drop n s)]
                         f _ [] = []
                         f [] _ = []
                         f [(V c1, s1)] [(V c2, s2)] = [(V (c1++c2), s1++s2)]

-- | FIXME: is able to simplified by list comprehension ??
-- 
variableN2 n s
  | length s < n  = []
  | otherwise     = let x = take n s
                        y = filter isAlpha x
                        t = x == y in
                    if t then f x else []
                    where f x = [(V x, drop n s)]
                    
variablem :: ReadS Var
variablem = map fchar . char isAlpha
            where fchar (c, s) = (V [c], s)

variablem2 :: ReadS Var
variablem2 s = concatMap f1 (variablem s)
               where f1 (V c1, s1) = map (f2 c1) (variablem s1)
                     f2 c1 (V c2, s2) = (V (c1++c2), s2)
                     
variablemN :: Int -> ReadS Var
variablemN n s 
  | length s >= n = variablemN' n s
  | otherwise     = []

variablemN' :: Int -> ReadS Var
variablemN' n s = foldr f1 init [1..n]
                  where init = [(V "", s)]
                        f1 _ [] = []       
                        f1 _ [(V cs1, s1)] = map (f2 cs1) (variablem s1)
                        f2 cs1 (V cs2, s2) = (V (cs1++cs2), s2)

{- Test via QuickCheck -}

prop_variable s = variable s == variablem s
prop_variable2 s = variable2 s == variablem2 s
prop_variableN s = variable2 s == variableN 2 s
prop_variableN2 s = variable2 s == variableN2 2 s
prop_variablemN s = variable2 s == variablemN 2 s

test = mapM_ quickCheck 
       [ prop_variable, 
         prop_variable2, 
         prop_variableN, 
         prop_variableN2,
         prop_variablemN ]
