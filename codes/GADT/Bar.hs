{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module GADT.Bar where

import Control.Exception

-- Following 2 definition have same meanings.

data SomeException = forall e. Exception e => SomeException e

data SomeException2 where
  SomeException2 :: Exception e => e -> SomeException2
