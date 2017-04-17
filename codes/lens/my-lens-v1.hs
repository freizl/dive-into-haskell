#!/usr/bin/env stack
{- stack
  runghc
  --resolver lts-8.8
  --package hspec
-}


{-# LANGUAGE OverloadedStrings #-}


{-
code from video https://skillsmatter.com/skillscasts/4251-lenses-compositional-data-access-and-manipulation
-}

module Main where

import Test.Hspec

data LensR s a = LensR { viewR :: s -> a
                       , setR :: a -> s -> s
                       -- , mod :: (a -> a) -> s -> s
                       -- , modM :: (a -> Maybe a) -> s -> Maybe s
                       -- , modIO :: (a -> IO a) -> s -> IO s
                       -- , ~~>> modF :: functor F => (a -> F a) -> s -> F s
                       }

compL :: LensR s t -> LensR t a -> LensR s a
compL (LensR f g) (LensR f' g') = LensR (f' . f) (\a s -> g (g' a (f s)) s)

data Person = Person { name :: String
                     , address :: Address
                     } deriving (Show, Eq)

data Address = Address { street :: String
                       , city :: String
                       , postcode :: Int
                       } deriving (Show, Eq)


name' :: LensR Person String
name' = LensR name (\n p -> p {name = n})

address' :: LensR Person Address
address' = LensR address (\a p -> p {address = a})

street' :: LensR Address String
street' = LensR street (\s p -> p {street = s})

city' :: LensR Address String
city' = LensR city (\c a -> a {city = c})

postcode' :: LensR Address Int
postcode' = LensR postcode (\p a -> a {postcode = p})


-- Test

testP :: Person
testP = Person "Simon" testA

testA :: Address
testA = Address "7100 San Ramon RD." "Dublin" 94455

main :: IO ()
main = hspec $ do

  describe "ViewR" $ do
    it "Person name is Simon" $
      viewR name' testP `shouldBe` "Simon"
    it "Address street is 7100 San Ramon" $
      viewR (compL address' street') testP `shouldBe` "7100 San Ramon RD."
    it "Address city is Dublin" $
      viewR (compL address' city') testP `shouldBe` "Dublin"
    it "Address postcode is 94455" $
      viewR (compL address' postcode') testP `shouldBe` 94455

  describe "setR" $ do
    it "Person name to Haisheng" $
      viewR name' (setR name' "Haisheng" testP) `shouldBe` "Haisheng"
    it "Address street to Grey lane" $ do
      let f = compL address' street'
      viewR f (setR f "Grey lane" testP) `shouldBe` "Grey lane"
    it "Address city is Dublin" $ do
      let f = compL address' city'
      viewR f (setR f "Pleasant Hill" testP) `shouldBe` "Pleasant Hill"
    it "Address postcode is 94455" $ do
      let f = compL address' postcode'
      viewR f (setR f 94550 testP) `shouldBe` 94550
