module Constant where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

{-
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
-}
newtype Constant a b = Constant {getConstant :: a}
  deriving (Eq, Show)

{-
  fmap :: (a -> b) -> f a -> f b
  == (x -> y) -> Constant a x -> Constant a y
-}
instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

{-

Interchange
    u <*> pure y = pure ($ y) <*> u

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-}
instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  -- Wrong implementation at first try
  -- (Constant _) <*> (Constant x) = Constant x
  -- the `b` is phantom type
  -- type `f (a -> b)` really is `Constant x (a -> b)`
  -- which pattern match as `Constant x`.
  (Constant x) <*> (Constant y) = Constant (x <> y)

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return (Constant a)

main :: IO ()
main = do
  quickBatch (applicative (Constant (First $ Just 3) :: Constant (First Int) ([Int], Sum Int, Product Int)))

