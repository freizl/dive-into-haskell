-- | 

module DividedBy where

data DividedResult = Result ( Integer, Integer ) | DividedByZero
  deriving Show

-- TODO: how to deal with negative number?
-- see if can draw insights from https://www.geeksforgeeks.org/modulo-operations-in-programming-with-negative-results/
--
-- dividedBy :: Integral a => a -> a -> DividedResult
dividedBy :: Integer -> Integer -> DividedResult
dividedBy num denom
  | toInteger denom == 0 = DividedByZero
  | otherwise = Result $ go num denom 0
  where sigNum = signum num * signum denom
        go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

main = do
  mapM_ print [ divMod ( 11 :: Integer ) 2
              , divMod ( -11 ) 2
              , divMod ( -11 ) ( -2 )
              , divMod 11 ( -2 )
              ]
  putStrLn "---------------"
  mapM_ print [ dividedBy ( 11 :: Integer ) 2
              , dividedBy ( -11 ) 2
              , dividedBy ( -11 ) ( -2 )
              , dividedBy 11 ( -2 )
              ]
