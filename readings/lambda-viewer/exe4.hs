module Main where

import Data.Char
import Types

char :: (Char -> Bool) -> ReadS Char
char f (c:s) | f c = [(c, s)]
char f _           = []

variable :: ReadS Var
variable s = [ (V [c], s1) | (c, s1) <- char isAlpha s ]

variable2 :: ReadS Var
variable2 s = [ (V [c1, c2], s2) | (c1, s1) <- char isAlpha s, 
                                   (c2, s2) <- char isAlpha s1 ]

variablem :: ReadS Var
variablem = map fchar . char isAlpha
            where fchar (c, s) = (V [c], s)

variablem2 :: ReadS Var
variablem2 s@(s1:s2:_) = let [(V c1, s1)] = variablem s
                             [(V c2, s2)] = variablem s1
                         in [(V (c1++c2), s2)]
variablem2 _ = []

variablemN :: Int -> ReadS Var
variablemN n s 
  | length s >= n = variablemN' n s
  | otherwise     = []

variablemN' :: Int -> ReadS Var
variablemN' n s = foldr f init [1..n]
                 where init = [(V "", s)]
                       f i [(V cs1, s1)] = let [(V c2, s2)] = variablem s1
                                           in [(V (cs1++c2), s2)]
