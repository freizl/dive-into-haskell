-- |

module Char where

import Data.Char

-- isUpper :: Char -> Bool
-- isLower :: Char -> Bool


filterUpper :: String -> String
filterUpper = filter isUpper

cap :: String -> String
cap [] = []
cap (x:xs) = toUpper x : xs

capAll :: String -> String
capAll = map toUpper

capFirst :: String -> Char
capFirst = toUpper . head
