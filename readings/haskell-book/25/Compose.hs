{-# LANGUAGE InstanceSigs #-}

-- Is not mandatory but figured it's helpful
-- when writing instance function when the signature is right there.
--

-- |
module Compose where

newtype Compose f g a = Compose {getCompose :: f (g a)}
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = Compose ((<*>) <$> f <*> a)

-- Compose (f (g f')) <*> Compose (f (g a))

instance (Monad f, Monad g) => Monad (Compose f g) where
  return = pure

  -- TODO:
  -- This page http://web.cecs.pdx.edu/~mpj/pubs/RR-1004.pdf
  -- explains in depth why it is not possible
  --
  (>>=) :: Compose f g a -> (a -> Compose f g b) -> Compose f g b
  (>>=) x y = undefined -- make compiler happy temporarily

  -- (Compose fga) >>= f1 = Compose $ do
  --   ga <- fga
  --   -- this fails, have to `lift` `g a` to `f a`
  --   a <- ga
  --   let (Compose fgb) = f1 a
  --   return fgb

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  -- foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap h (Compose fga) = foldMap (foldMap h) fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse :: Applicative h => (a -> h b) -> Compose f g a -> h (Compose f g b)
  traverse h (Compose fga) = Compose <$> traverse (traverse h) fga
