module Main where

import Data.List
import Number.Fibonacci(fibs)

main :: IO ()
main = print $ snd $ head $ dropWhile (\(x,y) -> (not . isLastNinePandigit "123456789") x) (zip fibs [1..])

bothNinePandigit digits n = isFirstNinePandigit digits n && isLastNinePandigit digits n

isLastNinePandigit  digits n = digits == sort (lastDigits 9 n)
isFirstNinePandigit digits n = digits == sort (firstDigits 9 n)

firstDigits k n = take k (show n)
lastDigits  k n = show (n `mod` 10^k)
