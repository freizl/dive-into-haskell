-- Informatics 1 - Functional Programming
-- Tutorial 1
--
-- Due: the tutorial of week 3 (4/5 Oct.)

import Data.Char
import Data.List
import Test.QuickCheck



-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [ x `div` 2 | x <- xs, isEven x]

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs)
  | isEven x  = x `div` 2 : halveEvensRec xs
  | otherwise = halveEvensRec xs

-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensRec xs



-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [ x | x <- xs, x > lo && x < hi ]
                   -- NOTES: `&&` is not necessary here.
                   -- [x | x <- xs, lo <= x, x <= hi]

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec _ _ [] = []
inRangeRec lo hi (x:xs)
  | x > lo && x < hi  = x : inRangeRec lo hi xs
  | otherwise         = inRangeRec lo hi xs

-- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRange lo hi xs == inRangeRec lo hi xs



-- 3. sumPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives xs = length [ x | x <- xs, x > 0]

-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec = f 0
                    where f a [] = a
                          f a (x:xs)
                            | x > 0 = f (a+1) xs
                            | otherwise = f a xs
                    -- NOTES: inner function is not necessary.

-- Mutual test
prop_countPositives :: [Int] -> Bool
prop_countPositives xs = countPositives xs == countPositivesRec xs



-- 4. pennypincher

-- Helper function
discount :: Int -> Int
discount =  round . (* 0.9) . fromIntegral

reasonable :: Int -> Bool
reasonable = (<= 19900)

-- List-comprehension version
pennypincher :: [Int] -> Int
pennypincher xs = sum [ y | y <- [ discount x | x <- xs], reasonable y ]

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec [] = 0
pennypincherRec xs = f 0 xs
                     where f s [] = s
                           f s (x:xs) = let d = discount x in
                                        if reasonable d then f (s+d) xs else f s xs
                           -- NOTES: inner function is not necessary.

-- Mutual test
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincher xs == pennypincherRec xs



-- 5. sumDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits xs = product [ digitToInt x | x <- xs, isDigit x]

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec = f 1
                where f p [] = p
                      f p (x:xs) = if isDigit x
                                   then f (p*digitToInt x) xs
                                   else f p xs
                      -- NOTES: inner function is not necessary.

-- Mutual test
prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs == multDigitsRec xs


-- 6. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise [] = []
capitalise (x:xs) = toUpper x : [ toLower y | y <- xs ]

-- Recursive version
capitaliseRec :: String -> String
capitaliseRec [] = []
capitaliseRec (x:xs) = toUpper x : f xs
                       where f [] = []
                             f (y:ys) = toLower y : f ys

-- Mutual test
prop_capitalise :: String -> Bool
prop_capitalise xs = capitalise xs == capitaliseRec xs



-- 7. title

-- List-comprehension version
title :: [String] -> [String]
title [] = []
title (x:xs) = capitalise x : [ toCapOrLower y | y <- xs ]

toCapOrLower xs = let l = length xs in
                  if l > 3
                  then capitalise xs
                  else toLowers xs

toLowers = map toLower

-- Recursive version
titleRec :: [String] -> [String]
titleRec [] = []
titleRec (x:xs) = capitalise x : f xs
                  where f [] = []
                        f (y:ys) = toCapOrLower y : f ys

-- mutual test
prop_title :: [String] -> Bool
prop_title xs = title xs == titleRec xs




-- Optional Material

-- 8. crosswordFind

hasChar :: Char -> Int -> String -> Bool
hasChar c p str = str !! p == c

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind c p l xs
  | xs == [] = []
  | p < 0 = []
  | l < 0 = []
  | p + 1 >= l = []
  | otherwise = [ x | x <- xs, length x == l && hasChar c p x ]

-- Recursive version
crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordFindRec c p l xs
  | xs == [] = []
  | p < 0 = []
  | l < 0 = []
  | p + 1 >= l = []
  | otherwise = let y = head xs
                    ys = tail xs
                    b = length y == l && hasChar c p y in
                if b
                then y : crosswordFindRec c p l ys
                else crosswordFindRec c p l ys

-- Mutual test
prop_crosswordFind :: Char -> Int -> Int -> [String] -> Bool
prop_crosswordFind c p l xs = crosswordFind c p l xs == crosswordFindRec c p l xs



-- 9. search

-- List-comprehension version

search :: String -> Char -> [Int]
search xs c = [ y2 | (y1,y2) <- zip xs [0..], y1 == c ]

-- Recursive version
searchRec :: String -> Char -> [Int]
searchRec xs c = f 0 [] xs
                 where f i ys [] = ys
                       f i ys (z:zs) = if z == c
                                       then f (i+1) (ys++[i]) zs
                                       else f (i+1) ys zs

-- Mutual test
prop_search :: String -> Char -> Bool
prop_search xs c = search xs c == searchRec xs c


-- 10. contains

-- List-comprehension version
contains :: String -> String -> Bool
contains [] [] = True
contains xs ys 
  | length xs < length ys = False
  | otherwise = let hi = length xs - length ys + 1
                    possibles = [ drop i xs | i <- [1..hi] ]
                    bools = [ b | b <- possibles, ys `isPrefixOf` b ] in
                length bools > 0
                    

-- Recursive version
containsRec :: String -> String -> Bool
containsRec [] [] = True
containsRec xs ys 
  | length xs < length ys = False
  | otherwise = let b = ys `isPrefixOf` xs in
                if b then True else containsRec (drop 1 xs) ys
                -- NOTES: any elegant way to improve if.then.else part???
                --        check solution

-- Mutual test
prop_contains :: String -> String -> Bool
prop_contains xs ys = contains xs ys == containsRec xs ys
