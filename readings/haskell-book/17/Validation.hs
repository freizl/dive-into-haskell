module Validation where

import Test.QuickCheck (Arbitrary (..), elements)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

-- This is different
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- (pure) :: Applicative f => a -> f a
--
instance Monoid e => Applicative (Validation e) where
  -- how to determine to use Failure or Success? let GHC does it.
  -- pure :: a -> f a
  -- pure :: a -> Validation e a
  -- hence it has to be Success which combines the `a`.
  pure = Success
  (Success f) <*> (Success x) = Success (f x)
  (Failure e1) <*> (Failure e2) = Failure (e1 <> e2)
  (Failure e) <*> _ = Failure e
  _ <*> (Failure e) = Failure e

instance (Eq a, Eq e) => EqProp (Validation e a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    elements [Failure e, Success a]

main :: IO ()
main = do
  quickBatch (applicative (undefined :: Validation String (Int, Int, Char)))
