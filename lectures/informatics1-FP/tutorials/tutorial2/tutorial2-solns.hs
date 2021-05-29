-- Informatics 1 - Functional Programming 
-- Tutorial 2
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

import Data.Function
import Data.Maybe

-- 1.
rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
-- to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)

-- 3. 
alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp ch [] = ch
lookUp ch ((key,val):restKey)
    | key == ch = val
    | otherwise = lookUp ch restKey

-- alternative solution with list-comprehension
--
-- lookUp ch xs = head ([ y | (x,y) <- xs, x == ch] ++ [ch])

-- 5.
encipher :: Int -> Char -> Char
encipher k ch = lookUp ch (makeKey k)

-- 6.
normalize :: String -> String
normalize [] = []
normalize (ch:str)
    | isAlpha ch = toUpper ch : normalize str
    | isDigit ch = ch : normalize str
    | otherwise  = normalize str

-- 7.
encipherStr :: Int -> String -> String
encipherStr k str = [encipher k ch | ch <- normalize str]

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey key = [(b, a) | (a, b) <- key]

-- 9.
decipher :: Int -> Char -> Char
decipher k ch = lookUp ch (reverseKey (makeKey k))

decipherStr :: Int -> String -> String
decipherStr k str = [decipher k ch | ch <- str, isUpper ch || isDigit ch || ch == ' ']

-- 10.
prop_cipher :: Int -> String -> Property
prop_cipher k str = l ==> decipherStr k (encipherStr k str) == normalize str
    where l = (0 <= k && k <= 26)

-- You might notice that this causes quickCheck "give up" after passing less than 100 tests. Note that it shouldn't fail any!
-- A beter way of restricting the test cases for this particular example is to use mod to convert any Int to one that is in range:

prop_cipher' :: Int -> String -> Bool
prop_cipher' k str = decipherStr l (encipherStr l str) == normalize str
  where l = k `mod` 26

-- 11.
contains :: String -> String -> Bool
contains _ [] = True
contains [] _ = False
contains str substr = 
    isPrefixOf substr str || contains (tail str) substr

-- 12.
candidates :: String -> [(Int, String)]
candidates str = [(i, decipherStr i str) | i <- [0..25], candidate (decipherStr i str)]
    where candidate str = str `contains` "AND" || str `contains` "THE"

-- Optional Material
          
-- Optional Material

-- 13.

splitEachFive :: String -> [String]
splitEachFive xs | length xs > 5 = take 5 xs : splitEachFive (drop 5 xs)
                 | otherwise     = [ fillToFive xs ]

fillToFive :: String -> String
fillToFive xs = xs ++ replicate (5 - length xs) 'X'

-- An alternative solution demonstrating 'repeat'
fillToFive' :: String -> String
fillToFive' xs = take 5 (xs ++ repeat 'X')

-- 14.

prop_transpose :: String -> Bool
prop_transpose xs = ys == transpose (transpose ys)
    where
      ys = splitEachFive xs

-- The following example shows why 'transpose' is not
--  invertible in general. The transpose function
--  takes the 'columns' of a list of lists, and makes
--  them the 'rows' of a new list of lists. 
--
-- [[o n e],           [[o t t f f],       [[o n e e e],
--  [t w o],            [n w h o i],        [t w o r],  
--  [t h r e e],   -->  [e o r u v],   -->  [t h r e],  
--  [f o u r],          [e r e],            [f o u], 
--  [f i v e]   ]       [e],        ]       [f i v]     ]   


-- 15.

encrypt :: Int -> String -> String
encrypt n str = concat (transpose (splitEachFive (encipherStr n str)))

-- 16.

splitFiveWays :: String -> [String]
splitFiveWays xs | n `mod` 5 == 0 = splitEach (n `div` 5) xs
                 | otherwise      = error "splitFiveWays: not a multiple of 5"
                 where n = length xs

splitEach :: Int -> String -> [String]
splitEach _ [] = []
splitEach n xs = take n xs : splitEach n (drop n xs)

decrypt :: Int -> String -> String
decrypt n str = concat (transpose (splitFiveWays (decipherStr n str)))

-- Challenge.

-- Increment a frequency in a character frequency list.
incAsc :: Char -> [(Char, Int)] -> [(Char, Int)]
incAsc c [] = [(c,1)]
incAsc c ((d,n) : xs) | c == d    = (d, n+1) : xs
                      | otherwise = (d, n)   : incAsc c xs

countFreqs :: String -> [(Char, Int)]
countFreqs xs = count xs []
    where count [] freqs       = freqs
          count (x : xs) freqs = count xs (incAsc x freqs)

-- Rank the candidates which might decipher to 'E'
freqCandidates :: [Char] -> [Int]
freqCandidates xs = [ (ord c - ord 'E') `mod` 26 | (c, _) <- freqChars ]
    where freqChars     = sortBy p (countFreqs xs)
          p (_,m) (_,n) = compare n m

freqDecipher :: String -> [String]
freqDecipher xs = [ decrypt n xs | n <- freqCandidates xs ]