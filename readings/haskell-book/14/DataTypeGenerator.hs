-- |
module DataTypeGenerator where

import Test.QuickCheck

data Fool = Fulse | Frue deriving (Eq, Show)

foolGen1 :: Gen Fool
foolGen1 = elements [Fulse, Frue]

foolGen2 :: [(Int, Gen Fool)]
foolGen2 =
  [ (9, return Fulse),
    (1, return Frue)
  ]

foolGen3 :: Gen Fool
foolGen3 = oneof [return Fulse, return Frue]

instance Arbitrary Fool where
  -- arbitrary = foolGen
  --
  -- frequency :: [(Int, Gen a)] -> Gen a
  -- Chooses one of the given generators, with a weighted random distribution. The input list must be non-empty.
  arbitrary = frequency foolGen2

main :: IO ()
main = do
  xs <- sample' (arbitrary :: Gen Fool)
  print xs
