{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Data.List
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.IO.Class

main :: IO ()
main = putStrLn "Hello, Haskell!"

---
-- * StateT
---
-- Compare with default behavior (see `26.org`),
-- what's the problem with this implementation?
-- 1. given the `runStateT`, has to add `Monad m` constrain.
-- 2. shall the signature be `(s' -> s) -> StateT s m a -> StateT s' m a`
--    ^ NO. `s` is passing through all the way hence has to be same type.
--           runStateT :: StateT s m a -> s -> m (a, s)
--
withStateT2 :: Monad m => (s -> s) -> StateT s m a -> StateT s m a
withStateT2 f m = StateT $ \s -> do
  (a1, s1) <- runStateT m (f s)
  return (a1, s1)

s1 :: StateT Int IO Bool
s1 = StateT $ \s -> return (s > 0, s)

s2 :: StateT String IO Bool
s2 = StateT $ \s -> do
  putStrLn "s2"
  return ("hw" `isSubsequenceOf` s, "<hello world>" ++ s)

t2 :: IO ()
t2 = do
  let f = (++ " :D: ") :: String -> String
  let st = withStateT f s2
  (a, s) <- runStateT st "init hw statet"
  print (a, s)

t1 :: IO ()
t1 = do
  let f = (+ 3)
  let st = withStateT2 f s1
  (a, s) <- runStateT st 1
  print (a, s)


---
-- * Reader
---

-- why it is `r' -> r` instead of `r -> r'`
-- this is only feasible way.
-- given a function `r -> r`, it is intend to modify `r`
-- hence modify the `r` before password `m` (2nd arguments).
--
-- runReaderT :: ReaderT r m a -> r -> m a
-- see the `r` has been 'dropped'
-- v.s.
-- runStateT :: StateT s m a -> s -> m (a, s)
-- the `s` will be pass through
--
withReaderT2 :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT2 f m = ReaderT $ \r -> runReaderT m (f r)

---
-- * Multiple layers
---

{-

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

-}

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT $ ExceptT $ ReaderT $ const $ return (Right (Just 1))

-- instance (MonadIO m) => MonadIO (MaybeT m) where
--   liftIO ioa = MaybeT $ liftIO $ Just <$> ioa

-- instance (MonadIO m) => MonadIO (ReaderT r m) where
--   liftIO ioa = ReaderT $ const (liftIO ioa)

-- instance (MonadIO m) => MonadIO (StateT s m) where
--   liftIO ioa = StateT $ \s -> do
--     a <- liftIO ioa
--     return (a, s)
