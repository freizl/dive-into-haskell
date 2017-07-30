{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module Foo where

data Expr a where
  I :: Int -> Expr Int
  B :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  LessThan :: Expr Int -> Expr Int -> Expr Bool
  Cond :: Expr Bool -> Expr a -> Expr a -> Expr a

deriving instance Eq a => Eq (Expr a)
deriving instance Show a => Show (Expr a)

eval :: Expr a -> a
eval (I i) = i
eval (B b) = b
eval (Add x y) = eval x + eval y
eval (LessThan x y) = eval x < eval y
eval (Cond c t f)
  | eval c = eval t
  | otherwise = eval f
