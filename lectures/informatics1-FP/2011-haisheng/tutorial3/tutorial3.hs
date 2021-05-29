-- Informatics 1 - Functional Programming
-- Tutorial 3
--
-- Week 5 - Due: 20/21 Oct.
--
-- Author: Haisheng Wu
--

import Data.Char
import Data.List
import Test.QuickCheck



-- 1. Map
-- a.
uppers :: String -> String
uppers = map toUpper

-- b.
doubles :: [Int] -> [Int]
doubles = map (* 2)

-- c.
penceToPounds :: [Int] -> [Float]
penceToPounds = map (\x-> fromIntegral x / 100.0)

-- d.
uppers' :: String -> String
uppers' xs = [toUpper x | x <- xs ]

prop_uppers :: String -> Bool
prop_uppers str = uppers str == uppers' str



-- 2. Filter
-- a.
alphas :: String -> String
alphas = filter isAlpha

-- b.
rmChar ::  Char -> String -> String
rmChar c = filter (/= c)

-- c.
above :: Int -> [Int] -> [Int]
above n = filter (> n)

prop_above :: Int -> [Int] -> Bool
prop_above n ns = all (> n) (above n ns)

-- d.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals = filter (uncurry (/=))

-- e.
rmCharComp :: Char -> String -> String
rmCharComp c xs = [ x | x <- xs, x /= c]

prop_rmChar :: Char -> String -> Bool
prop_rmChar c xs = rmChar c xs == rmCharComp c xs



-- 3. Comprehensions vs. map & filter
-- a.
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

upperChars' :: String -> String
upperChars' = map toUpper . filter isAlpha

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- b.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' = map (*2) . filter (>3)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs

-- c.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' = map reverse . filter (even . length)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

productFold :: [Int] -> Int
productFold = foldr (*) 1

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- b.
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold = foldr (&&) True

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs

-- c.
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (xs:xss) = xs ++ concatRec xss

concatFold :: [[a]] -> [a]
concatFold = foldr (++) []

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- d.
rmCharsRec :: String -> String -> String
rmCharsRec [] xs = xs
rmCharsRec _ [] = []
rmCharsRec (x:xs) str = rmCharsRec xs (rmChar x str)

rmCharsFold :: String -> String -> String
rmCharsFold xs str = foldr rmChar str xs

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str



type Matrix = [[Int]]


-- 5
-- a.
uniform :: [Int] -> Bool
uniform [] = True
uniform (x:xs) = all (== x) xs

-- b.
valid :: Matrix -> Bool
valid [] = False
valid [[]] = False
valid xss = uniform (map length xss)


-- 6.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [ uncurry f z | z <- zip xs ys ]

zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = map (uncurry f) (zip xs ys)

prop_zipwith :: [Int] -> [Int] -> Bool
prop_zipwith xs ys = zipWith (+) xs ys == zipWith' (+) xs ys

-- 7.
-- | FIXME: need validate matrix
plusM :: Matrix -> Matrix -> Matrix
plusM = zipWith (zipWith (+))

-- 8.
timesM :: Matrix -> Matrix -> Matrix
timesM [] _ = []
timesM _ [] = []
timesM xss yss
  | length (head xss) /= length yss = error "time matrix: invalid input"
  | otherwise = [ [ dot xs ys | ys <- transpose yss] | xs <- xss ]

dot :: [Int] -> [Int] -> Int
dot xs ys = sum $ zipWith (*) xs ys

-- Optional material
-- 9.
