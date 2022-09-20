{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module HelloBuildInTypes where

import Data.Char
import Data.Proxy
import GHC.TypeLits

data Foo1 (s :: Symbol) a = Foo1 String
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
  -- TODO: if Foo1 holds string, does it make sense to
  -- make sure value at term and type stay at sync?
  -- Well, they are exists at different layer, term and type
  -- unless to have a smart contract to create Foo1.
  -- e.g. mkFoo1 str = Foo1 str :: Foo1 xxx Int
  -- but  ↑ possible??
  let s = getSym a ++ getSym b
  in
    Foo1 "test"

-- usually no correlation between term and type level
-- the @MkList@ sort connects the size of list to type level.
--
data MyList (n :: Nat) a where
  Nil :: MyList 0 a
  MkList :: a -> MyList x a -> MyList (x+1) a

-- | Compile error
-- l0 :: MyList 1 Int
-- l0 = Nil

l1 :: MyList 1 Int
l1 = MkList 10 Nil

l2 :: MyList 2 Int
l2 = MkList 10 (MkList 20 Nil)

{-
the method does not compile
see <https://stackoverflow.com/questions/24734704/append-for-type-level-numbered-lists-with-typelits>
and <https://yav.github.io/publications/improving-smt-types.pdf>
and <https://github.com/yav/type-nat-solver>

TypeFamilyDependency
-}
-- appendList :: MyList x Int -> MyList y Int -> MyList (x+y) Int
-- appendList a Nil = a
-- appendList Nil b = b
-- appendList (MkList a as) bs = MkList a (as `appendList` bs)

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
