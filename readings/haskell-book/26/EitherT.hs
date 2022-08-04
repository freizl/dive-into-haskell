{-# LANGUAGE OverloadedStrings #-}

-- |
module EitherT where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad.Trans.Class

newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

-- instance Show (EitherT e m a) where
--   show _ = "EitherT (no idea how to write show instance)"

-- dont know how to write Eq instance
-- instance Eq (EitherT e m a ) where
--    a == b = True

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ma) = EitherT $ fmap (fmap f) ma

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure

  (EitherT mf) <*> (EitherT ma) = EitherT $ (<*>) <$> mf <*> ma

-- <*> :: f (a->b) -> f a -> f b
-- (\(x::Either e (a->b)) (y::Either e a) ->  x <*> y)   m (Either e (a->b))   m (Either e a)

instance Monad m => Monad (EitherT e m) where
  return = pure
  -- Does it matter one way or another?
  -- Personally I may start with 2nd
  (EitherT v) >>= f = EitherT $ v >>= either (return . Left) (runEitherT . f)
  -- (EitherT v) >>= f = EitherT $ do
  --   a <- v
  --   case a of
  --     Left e -> return (Left e)
  --     Right a -> runEitherT (f a)

instance MonadTrans (EitherT e) where
  lift ma = EitherT (Right <$> ma)

swapEither :: Either e a -> Either a e
swapEither (Right a) = Left a
swapEither (Left e) = Right e

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ea) = EitherT $ fmap swapEither ea

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT ab) = do
  v <- ab
  case v of
    Right b -> g b
    Left a -> f a

eitherT2 :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT2 f g (EitherT ab) = ab >>= either f g

instance (Arbitrary a, Arbitrary e, Monad m) => Arbitrary (EitherT e m a) where
  arbitrary = EitherT . return <$> arbitrary


-- instance (Eq e, Eq a, Monad m) => EqProp (EitherT e m a) where
--   (=-=) = eq

-- main :: IO ()
-- main = do
--   quickBatch (applicative (undefined :: EitherT String IO (Char, Int, Char)))
