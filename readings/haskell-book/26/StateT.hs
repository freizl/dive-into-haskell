-- |
module StateT where

import Control.Monad.Trans.Class

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance Functor m => Functor (StateT s m) where
  fmap f (StateT g) = StateT $ \s -> do
    fmap (\(a, s) -> (f a, s)) (g s)

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)

  (StateT ff) <*> (StateT fa) = StateT $ \s -> do
    (g, s1) <- ff s
    (a, s2) <- fa s1
    return (g a, s2)


{-
-- <*> :: f (a->b) -> f a -> f b
-- ff :: s -> f (a->b, s)
-- fa :: s -> f (a, s)
-- (\x y -> x y) <$> ff s <*> fa a
-- ($) <$> ff <*> fa


instance Applicative m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)

  (StateT ff) <*> (StateT fa) = StateT $ \s -> (\(f, s1) (a, s2) -> (f a, s)) <$> ff s <*> fa s

-- Cannot get this working given m is applicative.
-- hence official implementation is change m to be Monad.
--
-- stateAp :: StateT s m (a->b) -> StateT s m a -> StateT s m b
-- stateAp (StateT ff) (StateT fa) = StateT $
--   \s -> (\(f, s1) -> (\(a2, s2) -> (f a2, s2)) <$> fa s1 ) <$> ff s

-}


instance Monad m => Monad (StateT s m) where
  (StateT ma) >>= f = StateT $ \s -> do
    (a1, s1) <- ma s
    runStateT (f a1) s1

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do
    a <- ma
    return (a, s)
