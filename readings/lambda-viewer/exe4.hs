module Main where

import Data.Char
import Types
import Test.QuickCheck

main :: IO ()
main = test


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

-- | Test via QuickCheck
prop_variable2 s = variable2 s == variablem2 s
prop_variableN s = variable2 s == variablemN 2 s

test = mapM_ quickCheck [prop_variable2, prop_variableN]
