{-# LANGUAGE InstanceSigs #-}

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

  -- http://web.cecs.pdx.edu/~mpj/pubs/RR-1004.pdf
  (>>=) :: Compose f g a -> (a -> Compose f g b) -> Compose f g b
  (Compose fga) >>= f1 = Compose $ do
    ga <- fga
    a <- ga -- this fails, got to `lift` `g a` to `f a`
    let (Compose fgb) = f1 a
    return fgb
