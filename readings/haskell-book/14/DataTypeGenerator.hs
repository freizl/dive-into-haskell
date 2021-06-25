-- |
module DataTypeGenerator where

import Test.QuickCheck

data Fool = Fulse | Frue deriving (Eq, Show)

foolGen1 :: Gen Fool
foolGen1 = elements [Fulse, Frue]

foolGen2 :: [(Int, Gen Fool)]
foolGen2 =
  [ (2, elements [Fulse]),
    (1, elements [Frue])
  ]

instance Arbitrary Fool where
  -- arbitrary = foolGen
  arbitrary = frequency foolGen2

main :: IO ()
main = do
  sample (arbitrary :: Gen Fool)
