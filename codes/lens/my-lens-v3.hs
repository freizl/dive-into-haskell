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
import Data.Char

type Traversal' s a = forall f. Applicative f => (a -> f a) -> s -> f s

-- | newtype Const a b = Const { getConst :: a }
--
view :: Monoid a => Traversal' s a -> s -> a
view ln s = getConst (ln setField s)
  where setField :: a -> Const a a
        setField = Const

-- | newtype Identity a = Identity { runIdentity :: a }
--
set :: Traversal' s a -> (a -> s -> s)
set ln a s = runIdentity (ln setField s)
  where setField _ = Identity a

over :: Traversal' s a -> (a -> a) -> s -> s
over ln f s = runIdentity $ ln setField s
  where setField a = Identity (f a)


data Person = Person { name :: String
                     , address :: Address
                     } deriving (Show, Eq)

data Address = Address { street :: String
                       , city :: String
                       , postcode :: Int
                       } deriving (Show, Eq)

streetCity :: Traversal' Address String
streetCity f (Address s c p) =
  pure (\s' c' -> Address s' c' p)
  <*> f s
  <*> f c

-- Test

testA :: Address
testA = Address "7100 San Ramon RD." "Dublin" 94455

main :: IO ()
main = hspec $ do
  describe "View" $ do
    it "street name plus city" $
      view streetCity (over streetCity toLowerStr testA) `shouldBe` "7100 san ramon rd.dublin"


toLowerStr :: String -> String
toLowerStr = map toLower
