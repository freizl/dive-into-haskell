module Main where

import Data.Char
import Test.QuickCheck
import Types
import Control.Arrow (first)

{- Exercise 5 -}

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

(|||) :: ReadS a -> ReadS b -> ReadS (Either a b)
f ||| g = \s -> case f s of
                  [] -> map right (g s)
                  xs -> map left xs
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

(>>>+) :: ReadS [a] -> ReadS [a] -> ReadS [a]
f >>>+ g = \s -> [ (x++y, s2) | (x, s1) <- f s, (y, s2) <- g s1 ]

infixl 1 >>>=
infixl 1 >>>>
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

-- | re-implement of `reads :: ReadS` Int from Prelude.
--   1. `reads` ignore space at front
-- 
num :: ReadS Int
num = mapP read (many space >>>> many1 digits)

-- | re-implement of `reads :: Rational` Int from Prelude.
rational :: ReadS Rational
rational = mapP read rationalStr

-- | read a possible string for rational number.
--   Regex: [+-][0-9]+(%[0-9]+)?
--
rationalStr :: ReadS String
rationalStr = minusSign
              >>>+ nominator
              >>>+ percentSign
              >>>+ denominator
  where minusSign   = many (char (== '-'))
        nominator   = many1 digits
        percentSign = many1 (char (== '%'))
        denominator = many1 digits


isRationalChar :: Char -> Bool
isRationalChar c = isDigit c || c `elem` "%-+"


{- Test via QuickCheck -}

prop_num s = num s == (reads :: ReadS Int) s
prop_rational s = rational s == (reads :: ReadS Rational) s

test = mapM_ quickCheck [ prop_num
                        , prop_rational
                        ]
