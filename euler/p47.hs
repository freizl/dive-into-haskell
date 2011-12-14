module Main where

import Number.Primeing(primeFactors)
import Data.List(nub, intersect)

-- Question
-- 1. how to access tuple (1,"a","b")

main = print $ p47

upper = 1000
lower = 100
consentive = 3

p47   = filter isConsentive (candidates2 candidates)

candidates2 [] = []
candidates2 (x:y:[]) = []
candidates2 xs = (take 3 xs) : (candidates2 $ tail xs)
candidates = map (\x -> (x, (nub . primeFactors) x)) [lower..upper]

isConsentive xs@(x:y:z:[]) = isConsentive2 (length xs) (snd x) (snd y) (snd z)
isConsentive _           = False

isConsentive2 l xs ys zs = length xs == l
                        && length ys == l
                        && length zs == l
                        && (xs `intersect` ys) == []
                        && (zs `intersect` ys) == []
                        && (xs `intersect` zs) == []
                        
