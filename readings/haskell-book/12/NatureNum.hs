module NatureNum where

data Nat = Zero | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ s) = 1 + natToInteger s

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | i == 0 = Just Zero
  | otherwise = fmap Succ (integerToNat (i - 1))

t1 :: IO ()
t1 = do
  print $ natToInteger Zero
  print $ natToInteger (Succ Zero)
  print $ natToInteger (Succ (Succ Zero))
  print $ integerToNat 0
  print $ integerToNat 1
  print $ integerToNat 2
  print $ integerToNat (-1)
