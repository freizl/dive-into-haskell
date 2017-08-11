module MinFreeNumberSpec where

import Test.Hspec
-- TODO: to use quickcheck
import Test.QuickCheck

import MinFreeNumber

sample1, sample2 :: [Integer]
sample0 = []
sample1 = [08,23,09,0,12,11,10,13,07,41,04,14,21,05,17,03,19,02,06]
sample2 = [102..1000] ++ [0..100]
sample3 = [0]
sample4 = [1]
sample5 = [10]
sample6 = [10..15] ++ [0..8]

test :: SpecWith ()
test =
  describe "MinFreeNumber" $ do

    mapM_ runTest [ (sample0, 0)
                  , (sample1, 1)
                  , (sample2, 101)
                  , (sample3, 1)
                  , (sample4, 0)
                  , (sample5, 0)
                  , (sample6, 9)
                  ]
    --xit "returns identical result from minfree and minfree2" $ property $
      -- \xs -> minfree xs `shouldBe` minfree2 xs

runTest :: ([Integer], Integer) -> SpecWith (Arg Expectation)
runTest (sample, expected) =
  it ("returns " ++ show expected ++ " for input " ++ showSample sample) $ do
    minfree sample `shouldBe` expected
    minfree sample `shouldBe` minfree2 sample

showSample :: [Integer] -> String
showSample sample
 | length sample <= 5 = show sample
 | otherwise = show (take 5 sample) ++ "..."
