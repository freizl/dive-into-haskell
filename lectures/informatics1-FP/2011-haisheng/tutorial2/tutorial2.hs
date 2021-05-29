-- Informatics 1 - Functional Programming
-- Tutorial 2
--
-- Week 4 - due: 11/12 Oct.
--
-- Finished by: Haisheng Wu
--

import Data.Char
import Data.Ord (comparing)
import Data.List
import Test.QuickCheck


-- 1.
--rotate :: Int -> [Char] -> [Char]
rotate :: Int -> String -> String
rotate n str
  | n == 0 = str
  | n > 0 && n <= length str = rotate (n-1) (tail str ++ [head str])
  | otherwise = error "rotate: invalid input number"

-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3.
englishLetters = ['A'..'Z']
makeKey :: Int -> [(Char, Char)]
makeKey n = zip englishLetters (rotate n englishLetters)

-- 4. TODO: think about better solution: get ride of if.else or use recursion.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp2 c pairs = let found = [ y | (x, y) <- pairs, x == c ] in
                  if null found then c else head found
lookUp c [] = c
lookUp c ((a,b):xs)
  | c == a = b
  | otherwise = lookUp c xs

testLookup1 = lookUp 'B' [('A','F'), ('B','G'), ('C','H')] == 'G'
testLookup2 = lookUp '9' [('A','F'), ('B','G'), ('C','H')] == '9'

-- 5.
encipher :: Int -> Char -> Char
encipher n c = lookUp c (makeKey n)

-- 6.
normalize :: String -> String
normalize xs = [toUpper x | x <- xs, isLetter x || isDigit x ]

-- 7.
encipherStr :: Int -> String -> String
encipherStr n = map (encipher n) . normalize

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey xs = [ (y, x) | (x, y) <- xs ]

-- 9.
decipher :: Int -> Char -> Char
decipher n c = lookUp c (reverseKey $ makeKey n)

decipherStr :: Int -> String -> String
decipherStr n = map (decipher n)

-- 10.
prop_cipher :: Int -> String -> Property
prop_cipher n str = (n >= 0 && n <= length str)
                    ==> decipherStr n (encipherStr n str) == normalize str

-- 11.
contains :: String -> String -> Bool
contains [] _ = False
contains _ [] = True
contains str substr = substr `isPrefixOf` str
                      || contains (tail str) substr

-- 12.
candidates :: String -> [(Int, String)]
candidates encryptedStr = [ (n, str) | n <- [1..26],
                            let str = decipherStr n encryptedStr,
                            contains str "THE" || contains str "AND" ]



-- Optional Material

-- 13.
splitEachFive :: String -> [String]
splitEachFive str
  | length str <= 5 = [str ++ (replicate (5 - length str) 'X')]
  | otherwise = (take 5 str) : splitEachFive (drop 5 str)

-- 14.
prop_transpose :: String -> Property
prop_transpose str =
  (length str > 0 && length str `mod` 5 == 0)
  ==>
  (concat (transpose (transpose $ splitEachFive str)) == str)

-- 15.
encrypt :: Int -> String -> String
encrypt n = concat . transpose . splitEachFive . encipherStr n

-- 16.
-- TODO: is the solution incorrect?
decrypt :: Int -> String -> String
decrypt n str = decipherStr n . concat . transpose $
                splitEachN splitBy str
                where splitBy = length str `div` 5


splitEachN :: Int -> String -> [String]
splitEachN n str
  | length str <= n = [str ++ (replicate (n - length str) 'X')]
  | otherwise = (take n str) : splitEachN n (drop n str)

-- | FIXME: Doesnt work, how to get ride of appended 'X'??
prop_encrypt :: Int -> String -> Bool
prop_encrypt n str = decrypt n (encrypt n str) == str

-- Challenge (Optional)

-- 17.
countFreqs :: String -> [(Char, Int)]
countFreqs str = [(x, length xs) | xs@(x:_) <- group (sort str)]

-- 18
freqDecipher :: String -> [String]
freqDecipher str = map (\i -> decrypt i str) $
                   map (\(c, _) -> (ord c - ord 'E') `mod` 26) $
                   reverse $
                   sortBy (comparing snd) $
                   countFreqs str
