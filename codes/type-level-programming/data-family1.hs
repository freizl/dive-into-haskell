{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

data FactorA

data FactorB

data FactorC

data family FactorSettings a :: *

data instance FactorSettings FactorA = SA deriving (Show)

data instance FactorSettings FactorB = SB deriving (Show)

data instance FactorSettings FactorC = SC deriving (Show)

data FT a = forall a.
  (Show (FactorSettings a)) =>
  FT
  { factor :: a,
    settings :: FactorSettings a
  }

showFT :: FT a -> String
showFT FT {..} = show settings

main = print $ showFT $ FT (undefined :: FactorA) SA
