{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
-- | Haskell in Depth p369

module Main where

import Control.Exception

data family XList a

newtype instance XList () = XListUnit Integer
data instance XList Bool = XListBool Integer Integer

-- >>> :type XListBool 3 4
-- XListBool 3 4 :: XList Bool

-- compilation failure
-- data instance XList Bool = XListChar2 Char Char
-- data instance XList String = XListBool Char Char

data instance XList Char = XListChar Char Char
-- >>> :type XListChar 'a' 'b'
-- XListChar 'a' 'b' :: XList Char


-- >>> :type XListChar 3 4
-- No instance for (Num Char) arising from the literal `3'
-- In the first argument of `XListChar', namely `3'
-- In the expression: XListChar 3 4

main = putStrLn ""

data SomeException = forall e. Exception e => SomeException e

data SomeException2 where
  SomeException2 :: Exception e => e -> SomeException2
