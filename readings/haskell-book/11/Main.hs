module Main where

import Data.Char

main :: IO ()
main = putStrLn "Hello, Haskell!"


isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] [] = True
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf target@(x:xs) source@(y:ys)
  | x == y = isSubseqOf xs ys
  | otherwise = isSubseqOf target ys


capitalizeWords :: String -> [(String, String)]
capitalizeWords xs = map (\w -> (w, capWord w)) (words xs)

capWord :: String -> String
capWord [] = []
capWord (x:xs) = toUpper x : xs


capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph xs =
  let (x1, x2) = break (== '.') xs
      next = if length xs > 0 then (take 1 x2 ++ capitalizeParagraph (drop 1 x2)) else []
  in
    takeWhile isSpace x1 ++ capWord (dropWhile isSpace x1) ++ next
