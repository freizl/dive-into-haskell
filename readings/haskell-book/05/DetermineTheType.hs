{-# LANGUAGE NoMonomorphismRestriction #-}
-- |

module DetermineTheType where

-- example :: Integer
-- example :: Num a => a
example = 1

-- d1 :: Num a => a
d1 = (* 9) 6

-- d2 :: Num a => (a, [Char])
d2 = head [(0,"doge"),(1,"kitteh")]

-- d3 :: (Integer, [Char])
d3 = head [(0 :: Integer ,"doge"),(1,"kitteh")]

-- d4 :: Bool
d4 = if False then True else False

-- d5 :: Int
d5 = length [1, 2, 3, 4, 5]

-- d6 :: Bool
d6 = (length [1, 2, 3, 4]) > (length "TACOCAT")

-- w1 :: Num a => a
x1 = 5
y1 = x1 + 5
w1 = y1 * 10

-- z2 :: Num a => a -> a
x2 = 5
y2 = x2 + 5
z2 y = y * 10

-- f3 :: Fractional a => a
x3 = 5
y3 = x3 + 5
f3 = 4/y3

-- f4 :: [Char]
x4 = "Julie"
y4 = " <3 "
z4 = "Haskell"
f4 = x4 ++ y4 ++ z4
