-- |
module ReaderPlusMaybe where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Functor.Identity

{-
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

ask :: Monad m => ReaderT r m r

-}

handler :: Int -> String
handler i = show (i + 2) ++ "handler"

f1 :: ReaderT Int Maybe String
f1 = handler <$> ask

f2 :: ReaderT Int Maybe String
f2 = ReaderT $ const Nothing

t1 :: Maybe String
t1 = runReaderT f1 10

t2 :: Maybe String
t2 = runReaderT f2 10

b1 :: MaybeT (Reader Int) String
b1 = MaybeT (Just <$> reader handler)

b2 :: MaybeT (Reader Int) String
b2 = MaybeT (const Nothing <$> reader handler)

t3 :: Maybe String
t3 = runIdentity $ runReaderT (runMaybeT b1) 10

t4 :: Maybe String
t4 = runIdentity $ runReaderT (runMaybeT b2) 10
