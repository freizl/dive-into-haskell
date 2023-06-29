{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module ViewGegenic where

import GHC.Generics ( Generic )
import Data.Monoid ( Alt(Alt) )

-- ghci> :t coerce
-- coerce :: Coercible a b => a -> b

data Status = Ok | Err
    deriving (Eq, Generic)

newtype Age = Age Int
    deriving newtype (Eq)

newtype MAge = MAge (Maybe Int)
    deriving (Semigroup, Monoid) via (Alt Maybe Int)
