module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
    xs <- getArgs
    let tds = case xs of
          ("testdata":_) -> testdata
          ("testdata2":_) -> testdata2
          _ -> testdata
    mapM_ (print . runt tds) [pr1,pr2,pr3]
  where runt tds fn = map fn tds

-- | test data

testdata :: [Int]
testdata  = [123, 13, 100, 1000, 1111, 12345, 0, -10, 23]
testdata2 = [97093212, 1948102, 26549192, 5621892, 27043111, 56728181]

-- | ====================================== Solution One
-- | from http://www.artofproblemsolving.com/Forum/viewtopic.php?f=337&t=324693
data SepNum = SN { getA :: Int, 
                   getK :: Int, 
                   getM :: Int }
            deriving (Show)

-- | Build the number
buildNumber :: Int -> SepNum
buildNumber n = SN a k m
  where k = numberLength n - 1
        p = 10 ^ k
        a = n `div` p
        m = n `mod` p
  
-- | The length of a number.
numberLength :: Int -> Int
numberLength  = length . show

digitI :: Int
digitI = 1

occurences :: Int -> Int
occurences n
  | n <= 0 = 0
  | n < 10 = 1
  | otherwise = occurencesCountFormula digitI a k m
                where sn = buildNumber n
                      a = getA sn
                      k = getK sn
                      m = getM sn
        
-- | Ti(n) = Ti(a*10k + m)
--   i -> a -> k -> m -> Result
occurencesCountFormula :: Int -> Int -> Int -> Int -> Int 
occurencesCountFormula i a k m 
  | m == -1 && i < a     = a * k * (10 ^ (k-1)) + 10 ^ k
  | m == -1 && i >= a    = a * k * (10 ^ (k-1))
  | i < a                = occurencesCountFormula i a k (-1) + occurences m
  | otherwise            = occurencesCountFormula i a k (-1) + occurences m + m + 1

pr1 = occurences

-- | ====================================== Solution Two
-- | bruce force solution

pr2 :: Int -> Int
--pr2 n = sum $ concatMap (filter  (== 1) . splitNumber) [1..n]
pr2 n = length $ filter (== '1') $ concatMap show [1..n]

splitNumber :: Int -> [Int]
splitNumber n
  | n < 10   = [n]
  | otherwise = n `mod` 10 : splitNumber (n `div` 10)

-- | ====================================== Solution Three
--   by garriot@gmail.com
numbersByBit = 10 : zipWith (*) [9,9..] (map (10^) [1,2..])

-- | 10^n is not included!  onesWith10Power 2 means all 1 occurences in [0,1..100)
onesWithin10Power:: Int -> Int
onesWithin10Power 0 = 0
onesWithin10Power 1 = 1
onesWithin10Power 2 = 20
onesWithin10Power n = (countN + countPre) + onesWithin10Power (n -1)
    where 
        numbers = take (n-1) numbersByBit
        countN = last numbers * n 
        countPre = sum $ init numbers

-- | 1 occurences in [0, m * 10^n)  . onesWithinMulof10Power 2 3 means all 1 in [0, 300)
-- | mul must less than 10
onesWithinMulof10Power :: Int -> Int -> Int
onesWithinMulof10Power _ 0 = 0
onesWithinMulof10Power 0 mul = 1
onesWithinMulof10Power pow mul = onesWithin10Power pow * mul + ones mul
    where
        ones mul = if mul == 1
            then 0
            else 10^pow

-- | 1 occurences in [0, n). Because n is excluded [0,n), if you want 12345, please apply to (12345+1)
onesWithinAnyNumber :: Int -> Int
onesWithinAnyNumber n = checkTxtNumber txtNumber
    where
        txtNumber = show n
        checkTxtNumber [] = 0
        checkTxtNumber ['1'] = 1
        checkTxtNumber txt = if head txt == '1'
            then checkFirst (length txt -1) (readArray [head txt]) + readArray (tail txt) + checkTxtNumber (tail txt)
            else checkFirst (length txt -1) (readArray [head txt]) + checkTxtNumber (tail txt)
        checkFirst = onesWithinMulof10Power
        readOneChar c = read [c] :: Int
        readArray c = read c :: Int

pr3 x 
  | x <= 0 = 0
  | otherwise = 1 + onesWithinAnyNumber x
