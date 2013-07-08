module Main where

import Data.Char
import Test.QuickCheck
import Types
import Control.Arrow (first)

{- Exercise 5 -}


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
