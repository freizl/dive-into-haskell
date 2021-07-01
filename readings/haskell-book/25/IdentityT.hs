-- |
module IdentityT where

newtype IdentityT m a = IdentityT {runIdentityT :: m a} deriving (Eq, Show)

instance Functor m => Functor (IdentityT m) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (IdentityT ma) = IdentityT (f <$> ma)

instance Applicative m => Applicative (IdentityT m) where
  pure = IdentityT . pure
  (IdentityT mf) <*> (IdentityT ma) = IdentityT $ mf <*> ma

instance Monad m => Monad (IdentityT m) where
  return = pure
  -- (>>=) :: m a -> (a -> m b) -> m b
  (IdentityT ma) >>= f = IdentityT $ do
    a <- ma
    runIdentityT (f a)
  -- simplified to: IdentityT (ma >>= runIdentityT . f)
