-- Informatics 1 - Functional Programming 
-- Tutorial 1
--
-- Solutions
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!

import Data.Char
import Data.List
import Test.QuickCheck



-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]

-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs)
    | x `mod` 2 == 0 = x `div` 2 : halveEvensRec xs
    | otherwise      = halveEvensRec xs

-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensRec xs



-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec lo hi [] = []
inRangeRec lo hi (x:xs)
    | lo <= x && x <= hi  = x : inRangeRec lo hi xs
    | otherwise           = inRangeRec lo hi xs

-- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRange lo hi xs == inRangeRec lo hi xs



-- 3. sumPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives list = length [x | x <- list, x > 0]

-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs) | x > 0     = 1 + countPositivesRec xs
                         | otherwise = countPositivesRec xs

-- Mutual test
prop_countPositives :: [Int] -> Bool
prop_countPositives xs = countPositives xs == countPositivesRec xs


-- A list-comprehension version without library functions is not
-- possible, because list-comprehension examines items in a list
-- individually; it cannot use functions (like "+") between them.


-- 4. pennypincher

-- Helper function
discount :: Int -> Int
discount price = round ((fromIntegral price) * 0.9)

-- List-comprehension version.
pennypincher :: [Int] -> Int
pennypincher prices = sum [discount price | price <- prices, discount price <= 19900]

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec [] = 0
pennypincherRec (price:prices)
    | discount price <= 19900 = discount price + pennypincherRec prices
    | otherwise               = pennypincherRec prices


-- And the test itself
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincherRec xs == pennypincher xs



-- 5. sumDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits str = product [digitToInt ch | ch <- str, isDigit ch]

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs) | isDigit x  = digitToInt x * multDigitsRec xs
                     | otherwise  = multDigitsRec xs

-- Mutual test
prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs == multDigitsRec xs



-- 6. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise [] = []
capitalise (x:xs) = toUpper x : [toLower x | x <- xs]

-- Recursive version
capitaliseRec :: String -> String
capitaliseRec [] = []
capitaliseRec (x:xs) = toUpper x : lowerRec xs

lowerRec [] = []
lowerRec (x:xs) = toLower x : lowerRec xs


-- Mutual test
prop_capitalise :: String -> Bool
prop_capitalise xs = capitalise xs == capitaliseRec xs



-- 7. title

-- auxiliary functions used by both
capitaliseLong :: String -> String
capitaliseLong word | length word >= 4 = capitalise word
                    | otherwise        = lowercase word

lowercase :: String -> String
lowercase xs = [toLower x | x <- xs]

-- List-comprehension version
title :: [String] -> [String]
title [] = []
title (w:words) = capitalise w : [capitaliseLong w | w <- words]

-- Recursive version
titleRec :: [String] -> [String]
titleRec [] = []
titleRec (w:words) = capitaliseRec w : titleAuxRec words
  where titleAuxRec [] = []
        titleAuxRec (w:words) = capitaliseLong w : titleAuxRec words

capitaliseLongRec :: String -> String
capitaliseLongRec word | length word >= 4 = capitaliseRec word
                       | otherwise        = lowerRec word


-- mutual test
prop_title :: [String] -> Bool
prop_title xs = title xs == titleRec xs




-- Optional Material

-- 8. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words =
    [w | w <- words,  
         length w == len,
         0 <= pos,
         pos < len, 
         w !! pos == letter]

-- Recursive version
crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordFindRec letter pos len [] = []
crosswordFindRec letter pos len (w:ws) 
    | length w == len && 0 <= pos && pos < len && w !! pos == letter 
        = w : crosswordFindRec letter pos len ws
    | otherwise 
        = crosswordFindRec letter pos len ws

-- Mutual test
prop_crosswordFind :: Char -> Int -> Int -> [String] -> Bool
prop_crosswordFind letter pos len words =
   crosswordFind letter pos len words == crosswordFindRec letter pos len words



-- 9. search

-- List-comprehension version

-- This solution demonstrates the use of an infinite list [0..] of
-- indices. The "zip" function only zips pairs of elements together
-- as long as it has two elements to pair;  when it gets to the end
-- of one list, it stops.
search :: String -> Char -> [Int]
search str goal = [i | (ch, i) <- zip str [0..], ch == goal]

-- Recursive version
searchRec :: String -> Char -> [Int]
searchRec str goal = searchAux str goal 0
    where
      searchAux [] goal i = []
      searchAux (x:xs) goal i | x == goal  = i : searchAux xs goal (i+1)
                              | otherwise  = searchAux xs goal (i+1)

-- Mutual test
prop_search :: String -> Char -> Bool
prop_search str goal = search str goal == searchRec str goal



-- 10. contains

-- Get a list of all "tails" of a list
suffixes :: String -> [String]
suffixes xs = [drop i xs | i <- [0..length xs]]

-- Check these against the prefix and verify that there are
-- at least some that match
contains :: String -> String -> Bool
contains str substr = [] /= [ True | s <- suffixes str, isPrefixOf substr s ]

containsRec :: String -> String -> Bool
containsRec _ []       = True       -- otherwise contains "" "" gives `False'
containsRec [] substr  = False
containsRec str substr = isPrefixOf substr str || containsRec (tail str) substr

-- Mutual test
prop_contains :: String -> String -> Bool
prop_contains str substr = containsRec str substr == contains str substr



