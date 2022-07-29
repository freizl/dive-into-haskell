module First where

import Test.QuickCheck

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  Nada <> _ = Nada
  _ <> Nada = Nada
  (Only a) <> (Only b) = Only (a <> b)


instance Monoid a => Monoid (Optional a) where
  mempty = Nada

newtype First a = First {getFirst :: Optional a}
  deriving (Eq, Show)

instance Semigroup (First a) where
  (First Nada) <> (First Nada) = First Nada
  (First Nada) <> (First (Only x)) = First (Only x)
  a@(First (Only x)) <> _ = a

instance Monoid (First a) where
  mempty = First Nada

firstMappend :: Monoid a => First a -> First a -> First a
firstMappend = mappend

instance Arbitrary a => Arbitrary (First a) where
  arbitrary = (First . Only) <$> arbitrary

type FirstMappend =
     First String
  -> First String
  -> First String
  -> Bool
monoidAssoc :: FirstMappend
monoidAssoc a b c = (a `mappend` b) `mappend` c == a `mappend` (b `mappend` c)

type FstId = First String -> Bool
monoidLeftIdentity :: FstId
monoidLeftIdentity a = mempty `mappend` a == a

monoidRightIdentity :: FstId
monoidRightIdentity a = a `mappend` mempty == a

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

t1 :: IO ()
t1 = do
  let a = First (Only (1::Int))
  let b = First (Only (2::Int))
  let nada = First Nada
  print $ a `mappend` nada
  print (nada `mappend` nada :: First Int)
  print $ nada `mappend` b
  print $ a `mappend` b
