-- | 

module DividedBy where

data DividedResult = Result ( Integer, Integer ) | DividedByZero
  deriving Show

-- TODO: how to deal with negative number?
-- see if can draw insights from https://www.geeksforgeeks.org/modulo-operations-in-programming-with-negative-results/
--
-- Option 1 (turns out this is not how Haskell implement `div`)
-- a `div` b
-- a = bq + r ( 0 ≤ r < |b| )
-- start count (`q`) from 1 or -1 depends on the signum of dividend, dividend
-- calculate (dividend - divisor * count) until `r` is `>=0 && < |divisor|`
--
-- However, Haskell `divMod` doesn't always produce positive value for `r`.
-- From Haskell doc
-- quot is integer division truncated toward zero
-- div is integer division truncated toward negative infinity
--
sample :: [(Integer, Integer)]
sample = [ (8, 3)
         , (8, -3)
         , (-8, 3)
         , (-8, -3)
         -- , (9, 4)
         -- , (9, -4)
         -- , (-9, 4)
         -- , (-9, -4)
         ]

divUtil :: Show a
  => String
  -> (Integer -> Integer -> a)
  -> (Integer, Integer)
  -> String
divUtil name fn (a, b) =
  show a ++ " " ++ name ++ " " ++ show b ++
  " == "++ show (fn a b)

test1 :: IO ()
test1 = do
  mapM_ (putStrLn . divUtil "quotRem" quotRem) sample
  mapM_ (putStrLn . divUtil "divMod" divMod) sample

-- FIXME: still broken
-- dividedBy :: Integral a => a -> a -> DividedResult
dividedBy :: Integer -> Integer -> DividedResult
dividedBy num denom
  | toInteger denom == 0 = DividedByZero
  | otherwise = Result $
    if ( (signum num) * (signum denom) == 1 )
    then goP num denom 0
    else goN num denom 0 0
  where goN n d count sum
          | sum <= n = (count, sum - n)
          | otherwise    = goN n d (count - 1) (sum - (abs d))
        goP n d count
          | n < d = (count, n)
          | otherwise = goP (n - d) d (count + 1)

main :: IO ()
main = mapM_ (putStrLn . divUtil "dividedBy" dividedBy) sample
