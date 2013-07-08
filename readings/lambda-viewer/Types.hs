module Types where

import Data.Char
import Control.Arrow (first)

data Term = Var Var | Lam Var Term | App Term Term
          deriving (Show)
data Var  = V String
            deriving (Eq,Show)

------------------------------------------------------------
-- Basic

char :: (Char -> Bool) -> ReadS Char
char f (c:s) | f c = [(c, s)]
char f _           = []

space :: ReadS Char
space = char isSpace

digits :: ReadS Char
digits = char isDigit

nil :: ReadS [a]
nil s = [([], s)]

------------------------------------------------------------
-- convert char to XXX

charToVar :: Char -> Var
charToVar c = V [c]

charToInt :: Char -> Int
charToInt = digitToInt

------------------------------------------------------------
-- combinator

mapP :: (a -> b) -> ReadS a -> ReadS b
mapP f r = map (first f) . r
        -- map (\ (c, s) -> (f c, s)) . r

(&&&) :: ReadS a -> ReadS b -> ReadS (a, b)
f &&& g = \s -> [ ((x, y), s2) 
                | (x, s1) <- f s, 
                  (y, s2) <- g s1 ]

-- | Include all alternative cases
--
(|||) :: ReadS a -> ReadS b -> ReadS (Either a b)
f ||| g = \s -> map left (f s) ++ map right (g s)
          where left  (x, s) = (Left  x, s)
                right (y, s) = (Right y, s)


(<|>) :: ReadS a -> ReadS a -> ReadS a
f <|> g = mapP select (f ||| g)
  where select (Left  x) = x
        select (Right y) = y

-- | Basically, it is monadic `>>=` implementation.
--
(>>>=) :: ReadS a -> (a -> ReadS b) -> ReadS b
f >>>= g = \s -> [ (y, s2) | (x, s1) <- f s, (y, s2) <- g x s1 ]

(>>>>) :: ReadS a -> ReadS b -> ReadS b
f >>>> g = f >>>= const g

(=<<<) :: ReadS a -> (a -> ReadS b) -> ReadS a
f =<<< g = \s -> [ (x, s2) | (x, s1) <- f s, (y, s2) <- g x s1 ]

(<<<<) :: ReadS a -> ReadS b -> ReadS a
f <<<< g = f =<<< const g

(>>>+) :: ReadS [a] -> ReadS [a] -> ReadS [a]
f >>>+ g = \s -> [ (x++y, s2) | (x, s1) <- f s, (y, s2) <- g s1 ]

infixl 1 >>>=
infixl 1 >>>>
infixl 1 =<<<
infixl 1 <<<<
infixl 1 >>>+

many :: ReadS a -> ReadS [a]
many r  = many1 r <|> nil

many1 :: ReadS a -> ReadS [a]
many1 r = mapP cons (r &&& many r)
  where cons (x, xs) = x : xs

------------------------------------------------------------
-- parser

variable :: ReadS Var
variable = mapP charToVar (char isAlpha)

digit :: ReadS Int
digit = mapP charToInt (char isDigit)
