module Trivial where

import Data.Monoid
import Data.Semigroup
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

prop_semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
prop_semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup (Identity a) where
  a <> _ = a

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

data Two a b = Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

newtype BoolConj = BoolConj Bool
  deriving (Show, Eq)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  (BoolConj _) <> (BoolConj _) = BoolConj False

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> (arbitrary :: Gen Bool)

newtype BoolDisj = BoolDisj Bool
  deriving (Show, Eq)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  (BoolDisj _) <> (BoolDisj _) = BoolDisj True

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> (arbitrary :: Gen Bool)

data Or a b = Fst a | Snd b
  deriving (Show, Eq)

instance Semigroup (Or a b) where
  (Fst a) <> (Fst b) = Fst b
  (Snd a) <> (Fst b) = Snd a
  (Fst a) <> (Snd b) = Snd b
  (Snd a) <> (Snd b) = Snd b

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [Fst <$> arbitrary, Snd <$> arbitrary]

main :: IO ()
main = do
  quickCheck (prop_semigroupAssoc :: TrivAssoc)
  quickCheck (prop_semigroupAssoc :: IdentityAssoc (Product Int))
  quickCheck (prop_semigroupAssoc :: TwoAssoc (Sum Int) (Sum Int))
  quickCheck (prop_semigroupAssoc :: BoolConjAssoc)
  quickCheck (prop_semigroupAssoc :: BoolDisjAssoc)
  quickCheck (prop_semigroupAssoc :: OrAssoc Int Int)
