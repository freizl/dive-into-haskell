
import Data.List

main = do
  putStrLn "Input a n: "
  input <- getLine
  print $ fib (read input)

fib n = fibs !! n

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
