{-# LANGUAGE ExistentialQuantification #-}
module Error1 where

data Expr a
  = I Int
  | B Bool
  | Add (Expr a) (Expr a)
  | forall b. LessThan (Expr b) (Expr b)
  | forall c. Cond (Expr c) (Expr a) (Expr a)

int :: Int -> Expr Int
int = I

bool :: Bool -> Expr Bool
bool = B

add :: Expr Int -> Expr Int -> Expr Int
add = Add

lessThan :: Expr Int -> Expr Int -> Expr Bool
lessThan = LessThan

cond :: Expr Bool -> Expr a -> Expr a -> Expr a
cond = Cond

eval :: Expr a -> a
eval (I i) = i
eval (B b) = b
eval (Add x y) = eval x + eval y
eval (LessThan x y) = eval x < eval y
eval (Cond c t f)
  | eval c = eval t
  | otherwise = eval f
