module Main where

import Number.Primeing
import Data.List

main = print $ p41

p41          = maximum . head $ dropWhile null (map candidates possibleP)

possibleP    = ["987654321", "87654321", "7654321", "654321", "54321"]

candidates s = filter (isPrime . read) (permutations s)

isPrime n    = n:[] == primeFactors n

{--|

"7652413"

real	0m21.818s
user	0m21.761s
sys	0m0.016s

--}
