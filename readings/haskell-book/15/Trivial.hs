{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Trivial where

import Data.Monoid
import Data.Semigroup
import Test.QuickCheck
import Text.Show.Functions

prop_monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
prop_monoidLeftIdentity m = mempty `mappend` m == m

prop_monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
prop_monoidRightIdentity m = m `mappend` mempty == m

prop_semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
prop_semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

---

-- * Trivial

---

data Trivial = Trivial deriving (Eq, Show)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

type TrivialMonoidProp = Trivial -> Bool

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

---

-- * Identity

---
newtype Identity a = Identity a deriving (Eq, Show)

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

type IdentityMonoidProp a = Identity a -> Bool

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

---

-- * Two

---

data Two a b = Two a b
  deriving (Eq, Show)

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

type TwoMonoidProp a b = Two a b -> Bool

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

---

-- * BoolConj

---

newtype BoolConj = BoolConj Bool
  deriving (Show, Eq)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

type BoolConjMonoidProp = BoolConj -> Bool

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  (BoolConj _) <> (BoolConj _) = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> (arbitrary :: Gen Bool)

---

-- * BoolDisj

---

newtype BoolDisj = BoolDisj Bool
  deriving (Show, Eq)

type BoolDisjMonoidProp = BoolDisj -> Bool

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  (BoolDisj _) <> (BoolDisj _) = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> (arbitrary :: Gen Bool)

---

-- * Or

---

data Or a b = Fst a | Snd b
  deriving (Show, Eq)

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

instance Semigroup (Or a b) where
  (Fst a) <> (Fst b) = Fst b
  (Snd a) <> (Fst b) = Snd a
  (Fst a) <> (Snd b) = Snd b
  (Snd a) <> (Snd b) = Snd a

-- There is no way to make Or to be Monoid
-- because I can not find a mempty that obey the association law
--
-- instance Monoid a => Monoid (Or a b) where
--   mempty = Fst mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [Fst <$> arbitrary, Snd <$> arbitrary]

---

-- * Combine a b

---

newtype Combine a b = Combine {unCombine :: (a -> b)}
  deriving (Show)

type CombineAssoc a b = a -> Combine a b -> Combine a b -> Combine a b -> Bool

type CombineMonoidProp a b = a -> Combine a b -> Bool

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (\a -> f a <> g a)

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (\_ -> mempty)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

-- Unable to reuse @prop_semigroupAssoc@ as
-- @Combine@ has to be applied to argument
prop_semigroupCombine ::
  (Eq b, Semigroup b) =>
  a ->
  Combine a b ->
  Combine a b ->
  Combine a b ->
  Bool
prop_semigroupCombine x a b c =
  (unCombine $ a <> (b <> c)) x == (unCombine $ (a <> b) <> c) x

prop_combineLeftIdentity :: (Eq b, Monoid b) => a -> Combine a b -> Bool
prop_combineLeftIdentity x m =
  (unCombine $ mempty `mappend` m) x == (unCombine m) x

prop_combineRightIdentity :: (Eq b, Monoid b) => a -> Combine a b -> Bool
prop_combineRightIdentity x m =
  (unCombine $ m `mappend` mempty) x == (unCombine m) x

---

-- * Comp a

---

-- The book hints We can do something that seems a little more specific
-- due to input and output are same type.
-- TODO: I don't have an idea yet.
newtype Comp a = Comp {unComp :: a -> a}
  deriving (Show)

type CompAssoc a = a -> Comp a -> Comp a -> Comp a -> Bool

type CompMonoidProp a = a -> Comp a -> Bool

instance Semigroup a => Semigroup (Comp a) where
  Comp f <> Comp g = Comp (\a -> f a <> g a)

instance Monoid a => Monoid (Comp a) where
  mempty = Comp (\_ -> mempty)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp <$> arbitrary

prop_semigroupComp x a b c =
  (unComp $ a <> (b <> c)) x == (unComp $ (a <> b) <> c) x

prop_compLeftIdentity x m =
  (unComp $ mempty `mappend` m) x == (unComp m) x

prop_compRightIdentity x m =
  (unComp $ m `mappend` mempty) x == (unComp m) x

---

-- * Validation

---

data Validation a b = MyFailure a | MySuccess b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  MySuccess x <> _ = MySuccess x
  MyFailure x <> MySuccess y = MySuccess y
  MyFailure x <> MyFailure y = MyFailure (x <> y)

type ValidationAssoc a b = Validation a b -> Validation a b -> Validation a b -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [MyFailure a, MySuccess b]

main :: IO ()
main = do
  quickCheck (prop_semigroupAssoc :: TrivAssoc)
  quickCheck (prop_monoidLeftIdentity :: TrivialMonoidProp)
  quickCheck (prop_monoidRightIdentity :: TrivialMonoidProp)

  quickCheck (prop_semigroupAssoc :: IdentityAssoc (Product Int))
  quickCheck (prop_monoidLeftIdentity :: IdentityMonoidProp (Sum Int))
  quickCheck (prop_monoidRightIdentity :: IdentityMonoidProp (Sum Int))

  quickCheck (prop_semigroupAssoc :: TwoAssoc (Sum Int) (Product Int))
  quickCheck (prop_monoidLeftIdentity :: TwoMonoidProp (Product Int) (Sum Int))
  quickCheck (prop_monoidRightIdentity :: TwoMonoidProp (Sum Int) (Product Int))

  quickCheck (prop_semigroupAssoc :: BoolConjAssoc)
  quickCheck (prop_monoidLeftIdentity :: BoolConjMonoidProp)
  quickCheck (prop_monoidRightIdentity :: BoolConjMonoidProp)

  quickCheck (prop_semigroupAssoc :: BoolDisjAssoc)
  quickCheck (prop_monoidLeftIdentity :: BoolDisjMonoidProp)
  quickCheck (prop_monoidRightIdentity :: BoolDisjMonoidProp)

  quickCheck (prop_semigroupAssoc :: OrAssoc Int Int)

  quickCheck (prop_semigroupCombine :: CombineAssoc Int (Sum Int))
  quickCheck (prop_semigroupCombine :: CombineAssoc Int (Product Int))
  quickCheck (prop_combineLeftIdentity :: CombineMonoidProp Int (Sum Int))
  quickCheck (prop_combineRightIdentity :: CombineMonoidProp Int (Product Int))

  quickCheck (prop_semigroupComp :: CompAssoc (Product Int))
  quickCheck (prop_semigroupComp :: CompAssoc (Sum Int))
  quickCheck (prop_compLeftIdentity :: CompMonoidProp (Sum Int))
  quickCheck (prop_compRightIdentity :: CompMonoidProp (Sum Int))

  quickCheck (prop_semigroupAssoc :: ValidationAssoc String Int)
