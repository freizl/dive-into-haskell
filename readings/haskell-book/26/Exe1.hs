-- |
module Exe1 where

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Data.Functor.Identity
import Test.Hspec

rDec :: Num a => Reader a a
rDec = reader (+ (-1))

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT (Identity . show)

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
  print $ "Hi: " ++ show r
  return (r + 1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  print $ "Hi: " ++ show s
  return (show s, s + 1)

main :: IO ()
main = do
  putStrLn "== hspec test =="
  hspec testSpec
  putStrLn "== rPrintAndInc 1 =="
  runReaderT rPrintAndInc 1
  putStrLn "== rPrintAndInc [1..10] =="
  xs <- traverse (runReaderT rPrintAndInc) [1..10]
  print xs
  putStrLn "== rPrintAndAccum 10 =="
  runStateT sPrintIncAccum 10
  putStrLn "== rPrintAndAccum [1..5] =="
  ys <- mapM (runStateT sPrintIncAccum) [1..5]
  print ys

testSpec = do
  describe "rDec" $ do
    it "return 0 (rDec 1)" $
      runReader rDec 1 `shouldBe` 0
    it "return [0..9] (fmap rDec [1..10])" $
      fmap (runReader rDec) [1 .. 10] `shouldBe` [0 .. 9]
  describe "rShow" $ do
    it "return 1 (rShow 1)" $
      runIdentity (runReaderT rShow 1) `shouldBe` "1"
    it "return ['1'..'10'] (fmap rShow [1..10])" $
      fmap (runIdentity . runReaderT rShow) [1 .. 10] `shouldBe` [show x | x <- [1 .. 10]]
