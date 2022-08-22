{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | study notes from this video
-- https://youtu.be/FFZXWoqviBo
module ProxyVsTypeApplication where

import Data.Proxy

type family F a where
  F Int = Bool
  F Char = Double

-- * Proxy v.s. Type Application

g1 :: Proxy a -> F a -> ()
g1 _ _ = ()

x1 :: ()
x1 = g1 (Proxy :: Proxy Int) True

g2 :: F a -> ()
g2 _ = ()

x2 :: ()
x2 = g2 @Int True

-- * Higher kind

h1 :: (forall a. Proxy a -> F a -> ()) -> ()
h1 _ = ()

z1 :: ()
z1 = h1 (\(Proxy :: Proxy a) (_ :: F a) -> (undefined :: a) `seq` ())

h2 :: (forall a. F a -> ()) -> ()
h2 _ = ()

-- compilation error - syntax error
-- more details at https://gitlab.haskell.org/ghc/ghc/-/issues/17594
-- z2 = h2 (\ @a (_ :: F a) -> (undefined :: a) `seq` ())

-- compilation error
-- z2 = h2 (\ (_ :: F b) -> (undefined :: b) `seq` ())
