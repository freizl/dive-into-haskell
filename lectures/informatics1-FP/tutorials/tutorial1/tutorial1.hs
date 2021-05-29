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
halveEvens xs = undefined

-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec = undefined

-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens = undefined



-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = undefined

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec = undefined

-- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange = undefined



-- 3. sumPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives = undefined

-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec = undefined

-- Mutual test
prop_countPositives :: [Int] -> Bool
prop_countPositives = undefined



-- 4. pennypincher

-- Helper function
discount :: Int -> Int
discount = undefined

-- List-comprehension version
pennypincher :: [Int] -> Int
pennypincher = undefined

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec = undefined

-- Mutual test
prop_pennypincher :: [Int] -> Bool
prop_pennypincher = undefined



-- 5. sumDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits = undefined

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec = undefined

-- Mutual test
prop_multDigits :: String -> Bool
prop_multDigits = undefined



-- 6. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise = undefined

-- Recursive version
capitaliseRec :: String -> String
capitaliseRec = undefined

-- Mutual test
prop_capitalise :: String -> Bool
prop_capitalise = undefined



-- 7. title

-- List-comprehension version
title :: [String] -> [String]
title = undefined

-- Recursive version
titleRec :: [String] -> [String]
titleRec = undefined

-- mutual test
prop_title :: [String] -> Bool
prop_title = undefined




-- Optional Material

-- 8. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind = undefined

-- Recursive version
crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordFindRec = undefined

-- Mutual test
prop_crosswordFind :: Char -> Int -> Int -> [String] -> Bool
prop_crosswordFind = undefined 



-- 9. search

-- List-comprehension version

search :: String -> Char -> [Int]
search = undefined

-- Recursive version
searchRec :: String -> Char -> [Int]
searchRec = undefined

-- Mutual test
prop_search :: String -> Char -> Bool
prop_search = undefined


-- 10. contains

-- List-comprehension version
contains :: String -> String -> Bool
contains = undefined

-- Recursive version
containsRec :: String -> String -> Bool
containsRec = undefined

-- Mutual test
prop_contains :: String -> String -> Bool
prop_contains = undefined

