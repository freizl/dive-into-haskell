module Unfolds where

import Prelude hiding (iterate)

iterate :: (a -> a) -> a -> [a]
iterate f x = x: (iterate f (f x))

iterate2 :: (a -> a) -> a -> [a]
iterate2 f a = unfoldr (\x -> Just (x, f x)) a

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f x =
  case f x of
    Nothing -> []
    Just (a, b) -> a: (unfoldr f b)


main :: IO ()
main = do
  print (take 10 $ iterate (+ 1) 1 :: [Int])
  print (take 10 $ iterate (* 2) 2 :: [Int])
  print (take 10 $ iterate2 (+ 1) 1 :: [Int])
  print (take 10 $ iterate2 (* 2) 2 :: [Int])
  print (take 10 $ unfoldr (\b -> Just (b, b + 1)) 0)
  print (take 10 $ unfoldr (\b -> Just (b, b * 2)) 2)
  print (take 10 (unfoldr (\b -> Nothing) 1) :: [Int])
  print (unfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10)
