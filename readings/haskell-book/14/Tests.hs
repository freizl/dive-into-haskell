{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Map qualified as M
import Morse
import Test.Hspec
import Test.QuickCheck

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
  forAll
    charGen
    (\c -> ((charToMorse c) >>= morseToChar) == Just c)

main :: IO ()
main = hspec $ do
  describe "Morse" $ do
    it "there and back again" $ do
      property prop_thereAndBackAgain
