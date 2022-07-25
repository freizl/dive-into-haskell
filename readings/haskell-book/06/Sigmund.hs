-- |

module Sigmund where

import Data.List

-- myX :: Int
myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX

-- failure
sigmund2 :: a -> a
sigmund2 x = myX

-- failure
-- Int is only one case of Num but not all.
sigmund3 :: Num a => a -> a
sigmund3 x = myX

f :: Float
f = 1.0
f2 :: RealFrac a => a
f2 = 1.0

-- | failure
-- 1.0 can cover all possible Num
f3 :: Num a => a
f3 = 1.0


-- mySort :: [Char] -> [Char]
-- mySort = sort
-- signifier :: Ord a => [a] -> a
-- signifier xs = head (mySort xs)
