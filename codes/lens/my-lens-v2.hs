#!/usr/bin/env stack
{- stack
  runghc
  --resolver lts-8.8
  --package hspec
-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


{-
code from video https://skillsmatter.com/skillscasts/4251-lenses-compositional-data-access-and-manipulation
-}

module Main where

import Test.Hspec
import Data.Functor.Identity
import Control.Applicative

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- | newtype Const a b = Const { getConst :: a }
--
view :: Lens' s a -> s -> a
view ln s = getConst (ln setField s)
  where setField :: a -> Const a a
        setField = Const

-- | newtype Identity a = Identity { runIdentity :: a }
--
set :: Lens' s a -> (a -> s -> s)
set ln a s = runIdentity (ln setField s)
  where setField _ = Identity a

over :: Lens' s a -> (a -> a) -> s -> s
over ln f s = runIdentity $ ln setField s
  where setField a = Identity (f a)

setFree :: Lens' s a -> (a -> s -> s)
setFree ln x = runIdentity . ln (Identity . const x)


data Person = Person { name :: String
                     , address :: Address
                     } deriving (Show, Eq)

data Address = Address { street :: String
                       , city :: String
                       , postcode :: Int
                       } deriving (Show, Eq)


name' :: Lens' Person String
name' f p = fmap setx (f $ name p)
  where setx n = p {name = n}

address' :: Lens' Person Address
address' f p = fmap setx (f $ address p)
  where setx x = p {address = x}

street' :: Lens' Address String
street' f a = fmap setx (f $ street a)
  where setx x = a {street = x}

city' :: Lens' Address String
city' f a = fmap setx (f $ city a)
  where setx x = a {city = x}

postcode' :: Lens' Address Int
postcode' f a = fmap setx (f $ postcode a)
  where setx x = a {postcode = x}


-- Test

testP :: Person
testP = Person "Simon" testA

testA :: Address
testA = Address "7100 San Ramon RD." "Dublin" 94455

main :: IO ()
main = hspec $ do

  describe "View" $ do
    it "Person name is Simon" $
      view name' testP `shouldBe` "Simon"
    it "Address street is 7100 San Ramon" $
      view (address' . street') testP `shouldBe` "7100 San Ramon RD."
    it "Address city is Dublin" $
      view (address' . city') testP `shouldBe` "Dublin"
    it "Address postcode is 94455" $
      view (address' . postcode') testP `shouldBe` 94455

  describe "set" $ do
    it "Person name to Haisheng" $
      view name' (set name' "Haisheng" testP) `shouldBe` "Haisheng"
    it "Address street to Grey lane" $
      {-
         -- let f = address' . street' :: Lens' Person String
         TODO: compile error
         Ambiguous type variable ‘f0’ arising from an expression type signature
         prevents the constraint ‘(Functor f0)’ from being solved.
         Probable fix: use a type annotation to specify what ‘f0’ should be.
      -}
      view (address' . street') (set (address' . street') "Grey lane" testP) `shouldBe` "Grey lane"
    it "Address city is Dublin" $
      view (address' . city') (set (address' . city') "Pleasant Hill" testP) `shouldBe` "Pleasant Hill"
    it "Address postcode is 94455" $
      view (address' . postcode') (set (address' . postcode') 94550 testP) `shouldBe` 94550
