module Bases where

import Data.Char
import Types

char :: (Char -> Bool) -> ReadS Char
char f (c:s) | f c = [(c, s)]
char f _           = []

variable :: ReadS Var
variable s = [ (V [c], s1) | (c, s1) <- char isAlpha s ]

nil :: ReadS [a]
nil s = [([], s)]
