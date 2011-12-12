module Main where

import Data.List

main :: IO ()
main = do
  print $ findCertainFibIndex isFirstNinePandigit

findCertainFibIndex fn = length $ takeWhile (not . fn "123456789") fibs

bothNinePandigit digits n = (isFirstNinePandigit digits n) && (isLastNinePandigit digits n)

isLastNinePandigit  digits n = digits == (sort $ lastDigits 9 n)
isFirstNinePandigit digits n = digits == (sort $ firstDigits 9 n)


firstDigits k n = take k (show n)
lastDigits  k n = show (n `mod` 10^k)

--fib n = fibs !! n
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
