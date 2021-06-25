-- |

module QuickCheckExe where

import Data.List
import Test.QuickCheck

half :: Fractional a => a -> a
half x = x / 2

prop_half :: Float -> Bool
prop_half x = half x * 2 == x

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

prop_listOrdered1 :: [Int] -> Bool
prop_listOrdered1 = listOrdered . sort

square x = x * x
squareIdentify = square . sqrt

-- | doesn't hold
prop_squareIdentify :: Float -> Bool
prop_squareIdentify x
  | x <= 0 = True
  | otherwise = squareIdentify x == x

main :: IO ()
main = do
  quickCheck prop_half
  quickCheck prop_listOrdered1
  quickCheck prop_squareIdentify
