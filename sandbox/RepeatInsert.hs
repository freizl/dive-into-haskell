-- module RepeatInsert where
module Main where

main :: IO ()
main = do
  putStrLn "Insert into table(co1,co2) values"
  -- default show has extra character '[' and ']'
  putStrLn $ show dataValues

-- TODO how to read the types? (it is got from ghci via :type)
dataValues :: (Num t, Enum t, Num t1, Enum t1) => [(t, t1)]
dataValues = [ (i,j) | i <- [1..3], j <- [1..4]]
