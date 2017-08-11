module MinFreeNumberSpec where

import Test.Hspec
-- TODO: to use quickcheck
import Test.QuickCheck

import MinFreeNumber

sampleData1, sampleData2 :: [Integer]
sampleData0 = []
sampleData1 = [08,23,09,0,12,11,10,13,07,41,04,14,21,05,17,03,19,02,06]
sampleData2 = [102..1000] ++ [0..100]
sampleData3 = [0]
sampleData4 = [1]
sampleData5 = [10]
sampleData6 = [10..15] ++ [0..8]
sampleData7 = [0..8] ++ [10..18]

test :: SpecWith ()
test =
  describe "MinFreeNumber" $ do

    mapM_ runTest [ (sampleData0, 0)
                  , (sampleData1, 1)
                  , (sampleData2, 101)
                  , (sampleData3, 1)
                  , (sampleData4, 0)
                  , (sampleData5, 0)
                  , (sampleData6, 9)
                  , (sampleData7, 9)
                  ]
    --xit "returns identical result from minfree and minfree2" $ property $
      -- \xs -> minfree xs `shouldBe` minfree2 xs

runTest :: ([Integer], Integer) -> SpecWith (Arg Expectation)
runTest (sampleData, expected) =
  it ("returns " ++ show expected ++ " for input " ++ showSampleData sampleData) $ do
    minfree sampleData `shouldBe` expected
    minfree sampleData `shouldBe` minfree2 sampleData

showSampleData :: [Integer] -> String
showSampleData sampleData
 | length sampleData <= 5 = show sampleData
 | otherwise = show (take 5 sampleData) ++ "..."
