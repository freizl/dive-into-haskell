module Main where

-- TODO
-- 1. empty string

main :: IO ()
main = do
    xs <- sgetline
    mapM_ (\s -> putStrLn s) ([showSum xs , showProduct xs] ++ (map showFactorial xs))
  where 
    showSum x = "The sum is: " ++ (show . sum) x
    showProduct x = "The product is: " ++ (show . product) x
    showFactorial x = (show x) ++ " factorial is: " ++ (show . factorial) x
    
sgetline :: IO [Int]
sgetline = do
    putStrLn "Give me a number (or 0 to stop) :"  
    x <- getLine
    -- null to check if empty line
    -- non digit validate
    if x `elem` ["", "0"] then
      do return []
      else
      do xs <- sgetline
         return ((read x):xs)

factorial n = product [1..n]
