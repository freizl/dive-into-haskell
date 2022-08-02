module List where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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

  -- following implementation has flaw.
  -- though it works well for other laws but not composition.
  --
  -- _ <*> Nil = Nil
  -- (Cons f lf) <*> (Cons x lx) =
  --   let a = f x
  --       as = fmap f lx
  --       b = lf <*> pure x
  --       bs = lf <*> lx
  --   in (Cons a as) `append` (b `append` bs)

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

main :: IO ()
main = do
  quickBatch (applicative (undefined :: List (Int, Int, Char)))

t1 = do
  let f = Cons (+ 1) (Cons (* 2) Nil)
  let v = Cons 1 (Cons 2 Nil)
  print (f <*> v)

t4 = do
  let f = Cons (+ 1) (Cons (* 2) (Cons (+ 3) Nil))
  let v = Cons 1 (Cons 2 Nil)
  print (f <*> v)

-- | test case that could replay the failure that breaks composition law
t2 = do
  let u = Cons (+ 1) (Cons (* 2) Nil)
  let v = Cons (+ 3) (Cons (* 4) Nil)
  let w = Cons 1 (Cons 2 Nil)
  print (pure (.) <*> u <*> v <*> w)
  print (u <*> (v <*> w))

-- | test case that could replay the failure that breaks composition law
t3 = do
  let u = Cons (+ 1) (Cons (* 2) (Cons (+ 3) (Cons (* 4) Nil)))
  let v = Cons (+ 1) (Cons (* 2) Nil)
  let w = Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
  print (pure (.) <*> u <*> v <*> w)
  print (u <*> (v <*> w))
