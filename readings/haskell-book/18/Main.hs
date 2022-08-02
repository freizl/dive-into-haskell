module Main where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

---

-- * Nope a

---

data Nope a = NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  NopeDotJpg >>= f = f undefined

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

---

-- * BahEither b a

---

data BahEither b a = PLeft a | PRight b
  deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap _ (PRight b) = PRight b
  fmap f (PLeft a) = PLeft (f a)

instance Applicative (BahEither b) where
  pure = PLeft
  (PLeft f) <*> x = fmap f x
  (PRight a) <*> _ = PRight a

instance Monad (BahEither b) where
  return = pure
  (PLeft x) >>= f = f x
  (PRight x) >>= _ = PRight x

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = do
    b <- arbitrary
    a <- arbitrary
    elements [PLeft a, PRight b]

---

-- * Identity a

---

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

instance Monad Identity where
  (Identity a) >>= f = f a

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

---

-- * List

---
data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append xs Nil = xs
append (Cons x xs) ys = Cons x (xs `append` ys)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  (Cons f lf) <*> xs =
    let a = fmap f xs
        as = lf <*> xs
     in a `append` as

instance Monad List where
  Nil >>= _ = Nil
  (Cons a as) >>= f = f a `append` (as >>= f)

-- a :: a
-- as :: List a
-- f :: a -> List b
instance (Eq a) => EqProp (List a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    la <- arbitrary
    frequency
      [ (3, (return Nil) :: Gen (List a)),
        (5, return $ Cons a la)
      ]

main = do
  putStrLn "== Nope a =="
  quickBatch (monad (undefined :: Nope (Char, Int, Char)))
  putStrLn "\n== BahEither b a =="
  quickBatch (monad (undefined :: BahEither String (Char, Int, Char)))
  putStrLn "\n== Identity a =="
  quickBatch (monad (undefined :: Identity (Char, Int, Char)))
  putStrLn "\n== List a =="
  quickBatch (monad (undefined :: List (Char, Int, Char)))

---

-- * type driven implementation

---

j :: Monad m => m (m a) -> m a
j mma = mma >>= id

t1 = do
  print (j [[1, 2], [], [3]])
  print (j (Just (Just 1)))
  print (j (Just Nothing) :: Maybe Int)
  print (j Nothing :: Maybe Int)

-- fmap
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = ma >>= (return . f)

-- applicative
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = ma >>= (\a -> mb >>= (return . f a))


a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = do
  a <- ma
  f <- mf
  return (f a)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = do
  b <- f x
  bs <- meh xs f
  return (b: bs)

t2 = do
  print $ meh [1,3,5,7] Just -- Just [1,3,5,7]

flipType :: (Monad m) => [m a] -> m [a]
flipType mas = meh mas (\ma -> ma >>= return)

t3 = do
  print $ flipType [Just 2, Just 4, Just 8]
  print $ flipType [Just 2, Just 4, Just 8, Nothing]
