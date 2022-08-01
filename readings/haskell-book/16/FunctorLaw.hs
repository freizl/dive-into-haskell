{-# LANGUAGE ScopedTypeVariables #-}

module FunctorLaw where

import Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

t1 :: [Int] -> Bool
t1 xs = functorIdentity xs

t2 :: [Int] -> Bool
t2 xs = functorCompose (+ 1) (* 2) $ xs

main = do
  quickCheck t1
  quickCheck t2
  testIdentity
  testPair
  testTwo
  testThree
  testFour

---

-- * exercise

--
-- 16.10 Exercises: Instances of Func
---

f1 = (+ 11)

f2 = (* 23)

f3 c = c : "f3"

f4 = (++ "g4")

newtype Identity a = Identity a
  deriving (Eq)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

testIdentity = do
  quickCheck (\(a :: Int) -> functorIdentity (Identity a))
  quickCheck (\(a :: Int) -> functorCompose f1 f2 (Identity a))
  quickCheck (\(a :: Char) -> functorCompose f3 f4 (Identity a))

data Pair a = Pair a a
  deriving (Eq)

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

testPair = do
  quickCheck (\(a :: Int, b :: Int) -> functorIdentity (Pair a b))
  quickCheck (\(a :: Int, b :: Int) -> functorCompose f1 f2 (Pair a b))
  quickCheck (\(a :: Char, b :: Char) -> functorCompose f3 f4 (Pair a b))

data Two a b = Two a b
  deriving (Eq)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

testTwo = do
  quickCheck (\(a :: Int, b :: Char) -> functorIdentity (Two a b))
  quickCheck (\(a :: Char, b :: Int) -> functorIdentity (Two a b))
  quickCheck (\(a :: Int, b :: Char) -> functorCompose f3 f4 (Two a b))
  quickCheck (\(a :: Char, b :: Int) -> functorCompose f1 f2 (Two a b))

data Three a b c = Three a b c
  deriving (Eq)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b
  deriving (Eq)

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

testThree = do
  quickCheck (\(a :: Int, b :: Int, c :: Char) -> functorIdentity (Three a b c))
  quickCheck (\(a :: Char, b :: Int, c :: Int) -> functorIdentity (Three a b c))
  quickCheck (\(a :: Int, b :: Int, c :: Char) -> functorCompose f3 f4 (Three a b c))
  quickCheck (\(a :: Char, b :: Char, c :: Int) -> functorCompose f1 f2 (Three a b c))
  quickCheck (\(a :: Int, b :: Char, c :: Char) -> functorIdentity (Three' a b c))
  quickCheck (\(a :: Char, b :: Int, c :: Int) -> functorIdentity (Three' a b c))
  quickCheck (\(a :: Int, b :: Char, c :: Char) -> functorCompose f3 f4 (Three' a b c))
  quickCheck (\(a :: Char, b :: Int, c :: Int) -> functorCompose f1 f2 (Three' a b c))

data Four a b c d = Four a b c d
  deriving (Eq)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b
  deriving (Eq)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

testFour = do
  quickCheck (\(a :: String, b :: Int, c :: Char, d :: Char) -> functorIdentity (Four a b c d))
  quickCheck (\(a :: Char, b :: Bool, c :: Int, d :: Int) -> functorIdentity (Four a b c d))
  quickCheck (\(a :: Int, b :: Bool, c :: String, d :: Char) -> functorCompose f3 f4 (Four a b c d))
  quickCheck (\(a :: Char, b :: Bool, c :: String, d :: Int) -> functorCompose f1 f2 (Four a b c d))
  quickCheck (\(a :: Int, b :: Int, c :: Int, d :: Char) -> functorIdentity (Four' a b c d))
  quickCheck (\(a :: Char, b :: Char, c :: Char, d :: Int) -> functorIdentity (Four' a b c d))
  quickCheck (\(a :: Int, b :: Int, c :: Int, d :: Char) -> functorCompose f3 f4 (Four' a b c d))
  quickCheck (\(a :: Char, b :: Char, c :: Char, d :: Int) -> functorCompose f1 f2 (Four' a b c d))
