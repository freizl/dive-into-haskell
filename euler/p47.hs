module Main where

import Number.Primeing(primeFactors)
import Data.List(nub, find)

-- Question
-- 1. how to access tuple (1,"a","b")

main = print $ p47

lower = 600
count = 4

p47   = find isConsecutive (candidates distinctPrimeFactors)

candidates []         = []
candidates (x:y:[])   = []
candidates (x:y:z:[]) = []
candidates xs         = (take count xs) : (candidates $ tail xs)

distinctPrimeFactors = map (\x -> (x, (nub . primeFactors) x)) [lower..]

isConsecutive xs@(x:y:z:z1:[]) = countDistinct (length xs) (snd x) (snd y) (snd z) (snd z1)
isConsecutive _           = False

countDistinct l xs ys zs zs1 =  length xs == l
                        && length ys == l
                        && length zs == l
                        && length zs1 == l
                        
