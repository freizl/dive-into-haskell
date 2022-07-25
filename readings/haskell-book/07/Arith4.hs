-- |

module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip2 :: (Show a, Read a) => a -> a
roundTrip2 = read . show

roundTrip3 :: (Show a, Read b) => a -> b
roundTrip3 = read . show

main :: IO ()
main = do
  print (roundTrip 4)
  print (roundTrip2 4)
  print (roundTrip3 4 :: Integer)
  print (id 4)
