{-# LANGUAGE ScopedTypeVariables #-}
module Addition where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > (1 :: Int) `shouldBe` True
    it "2 + 2 is 4" $ do
      2 + 2 `shouldBe` (4 :: Int)
    -- quickCheck in hspec test
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
  describe "Multi (quickcheck)" $ do
    it "x `multi` 2 == x * 2" $ do
      property $ (\( x::Integer ) -> x `multi` 2 == x * 2)
    it "x `multi` 2 == x * 2 (10000 tests)" $ do
      withMaxSuccess 10000 $ (\( x::Integer ) -> x `multi` 2 == x * 2)

  describe "Multi" $ do
    it "1x1 = 1" $ do
      1 `multi` 1 `shouldBe` (1 :: Int)
    it "1x2 = 2" $ do
      1 `multi` 2 `shouldBe` ( 2::Int )
    it "2x1 = 1" $ do
      2 `multi` 1 `shouldBe` ( 2::Int )
    it "2x3 = 4" $ do
      2 `multi` 3 `shouldBe` ( 6::Int )
    it "(-2)x2 = 4" $ do
      (-2) `multi` 2 `shouldBe` (-4::Int)
    it "2x(-2) = -4" $ do
      2 `multi` (-2) `shouldBe` (-4::Int)

multi :: (Ord a, Eq a, Num a) => a -> a -> a
multi x y
  | x == 0 || y == 0 = 0
  | x == 1 = y
  | y == 1 = x
  | y < 0 = negate $ multi x (abs y)
  | otherwise = x + multi x (y - 1)

-- | arbitrary has type `Arbitrary a => Gen a`
-- hence have to specify type for `a` to dispatch to desired type class instance.
genInt :: Gen Int
genInt = arbitrary

-- | generate sample to console (stdout)
-- what is the use case for it?
--
sampleInt :: IO ()
sampleInt = sample genInt

sampleInt' :: IO [Int]
sampleInt' = sample' genInt

genDigits :: Gen Int
genDigits = elements [1 .. 10]

prop_multi :: Property
prop_multi =
  forAll
    genDigits
    (\x -> x `multi` 2 == x * 2)

runqc :: IO ()
runqc = do
  quickCheck prop_multi
