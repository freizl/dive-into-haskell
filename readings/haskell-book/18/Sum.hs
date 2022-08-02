-- |

module Sum where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b = First a | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  (Second f) <*> x = fmap f x
  (First a) <*> _ = First a

instance Monad (Sum a) where
  return = pure
  (Second x) >>= f = f x
  (First x) >>= _ = First x

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [First a, Second b]

main = do
  quickBatch (applicative (undefined :: Sum [Int] (Char, Int, Char)))
  quickBatch (monad (undefined :: Sum [Int] (Char, Int, Char)))

t1 = do
  let a = First [1,-2] :: Sum [Int] (Char -> Int)
  let b = First [-1,2] :: Sum [Int] Char
  print $ a `ap` b
  print $ a <*> b
