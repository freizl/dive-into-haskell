module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

{-
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
-}

{-

pure :: a -> [a]
(<*>) :: [ (a->b) ] -> [a] -> [b]

pure :: a -> IO a
(<*) :: IO (a->b) -> IO a -> IO b

pure :: a -> (x, a)
(<*) :: (x, a->b) -> (x, a) -> (x, b)

pure :: a -> (e -> a)
(<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)

(<*>) f g = \e -> (f e) (g e)

-}

---

-- * Pair

---
data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Applicative Pair where
  pure a = Pair a a
  Pair f g <*> Pair a1 a2 = Pair (f a1) (g a2)

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    return (Pair a1 a2)

---

-- * Two

---

data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  Two a1 f <*> Two a2 b2 = Two (a1 <> a2) (f b2)

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

---

-- * Three

---
data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  Three a1 b1 f <*> Three a2 b2 c2 = Three (a1 <> a2) (b1 <> b2) (f c2)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance (Monoid a) => Applicative (Three' a) where
  pure b = Three' mempty b b
  Three' a1 f g <*> Three' a2 b21 b22 = Three' (a1 <> a2) (f b21) (g b22)

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return (Three' a b1 b2)

---

-- * Four

---

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure d = Four mempty mempty mempty d
  Four a1 b1 c1 f <*> Four a2 b2 c2 d2 = Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (f d2)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Monoid a) => Applicative (Four' a) where
  pure b = Four' mempty mempty mempty b
  Four' a1 a2 a3 f <*> Four' a4 a5 a6 b1 = Four' (a1 <> a4) (a2 <> a5) (a3 <> a6) (f b1)

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    a3 <- arbitrary
    b <- arbitrary
    return (Four' a1 a2 a3 b)

main :: IO ()
main = do
  -- `applicative` doesn't evaluate the parameter but pull the type out of it.
  -- hence can pass `undefined`
  --
  putStrLn "== Pair a =="
  quickBatch (applicative (undefined :: Pair (Char, Char, Int)))
  putStrLn "\n== Two a b =="
  quickBatch (applicative (undefined :: Two [Char] (Char, Char, Int)))
  putStrLn "\n== Three a b c =="
  quickBatch (applicative (undefined :: Three [Char] [Int] (Char, Char, Int)))
  putStrLn "\n== Three' a b =="
  quickBatch (applicative (undefined :: Three' [Char] (Char, Char, Int)))
  putStrLn "\n== Four a b c d =="
  quickBatch (applicative (undefined :: Four [Char] [Int] [Char] (Char, Char, Int)))
  putStrLn "\n== Four' a b =="
  quickBatch (applicative (undefined :: Four' [Int] (Char, Char, Int)))

{-

Notes of Paper <Applicative programming with effects>
-}
iffy :: Applicative f => f Bool -> f a -> f a -> f a
iffy fc fa fb = cond <$> fc <*> fa <*> fb
  where
    cond c a b = if c then a else b

newtype Comp f g a = Comp {unComp :: f (g a)}
  deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (Comp f g) where
  fmap h (Comp fga) = Comp (fmap (fmap h) fga)

instance (Applicative f, Applicative g) => Applicative (Comp f g) where
  pure x = Comp (pure (pure x))

  {- how to understand the implementation

  -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  -- Comp f g (a -> b) <*> Comp f g a
  -- (\gf ga -> gf <*> ga) <$> f (g (a->b)) <*> f (g a)
  -}
  -- (Comp fs) <*> (Comp xs) = Comp $ (\gf ga -> gf <*> ga) <$> fs <*> xs
  (Comp fs) <*> (Comp xs) = Comp ((<*>) <$> fs <*> xs)

instance (Applicative f, Applicative g, Eq a, Eq (f (g a))) => EqProp (Comp f g a) where
  (=-=) = eq

-- How to write Arbitrary instance??
--
instance (Arbitrary1 f, Arbitrary1 g, Arbitrary a) => Arbitrary (Comp f g a) where
  arbitrary = Comp <$> liftArbitrary (liftArbitrary arbitrary)

testCompApplicative = do
  quickBatch (applicative (undefined :: Comp Maybe [] (Char, Int, Bool)))
