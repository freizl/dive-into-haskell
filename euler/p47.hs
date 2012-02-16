module Main where

import Number.Primeing(primeFactors)
import Data.List(nub, find)

-- Question
-- 1. how to access tuple (1,"a","b")

main = print $ p47

lower = 100
count = 4

p47 = find isConsecutive (makeCandidates distinctPrimeFactors)

makeCandidates    :: [(Integer, [Integer])] -> [[(Integer, [Integer])]]
makeCandidates xs = (take count xs) : (makeCandidates $ tail xs)

distinctPrimeFactors :: [(Integer, [Integer])]
distinctPrimeFactors = map (\x -> (x, (nub . primeFactors) x)) [lower..]

isConsecutive    ::  [(Integer,[Integer])] -> Bool
isConsecutive xs = and $ map ((== count) . length . snd) xs
                        
