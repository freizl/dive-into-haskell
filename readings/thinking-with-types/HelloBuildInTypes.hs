{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module HelloBuildInTypes where

import Data.Char
import Data.Proxy
import GHC.TypeLits

data Foo1 (s :: Symbol) a = Foo1
  deriving Show

t1 :: Foo1 "hello1" Char
t1 = undefined
t2 :: Foo1 "hello2" Char
t2 = undefined
t3 :: Foo1 "hello1" Char
t3 = undefined

getSym :: forall a s. KnownSymbol s => Foo1 s a -> String
getSym _ = symbolVal (Proxy @s)

appendSym ::
 forall s1 s2.
  (KnownSymbol s1, KnownSymbol s2) =>
  Foo1 s1 Char ->
  Foo1 s2 Char ->
  Foo1 (AppendSymbol s1 s2) Char
appendSym a b =
  -- if Foo1 holds string, does it make sense to
  -- make sure value at term and type stay at sync?
  let s = getSym a ++ getSym b
  in
    Foo1

sameSym1, sameSym2, sameSym3 ::
  forall s1 s2.
  (KnownSymbol s1, KnownSymbol s2) =>
  Foo1 s1 Char ->
  Foo1 s2 Char ->
  Bool
sameSym1 a b = (getSym a) == (getSym b)
sameSym2 _ _ = case cmpSymbol (Proxy @s1) (Proxy @s2) of
  EQI -> True
  _ -> False
sameSym3 _ _ = case sameSymbol (Proxy @s1) (Proxy @s2) of
  Just _ -> True
  Nothing -> False

-- sameSym2 ::
--   forall s1 s2.
--   (KnownSymbol s1, KnownSymbol s2) =>
--   Foo1 s1 Int ->
--   Foo1 s2 Int ->
--   CmpSymbol s1 s2
-- sameSym2 _ _ = undefined
