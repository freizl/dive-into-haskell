{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

data FactorA
data FactorB
data FactorC

class IsFactor a
instance IsFactor FactorA;
instance IsFactor FactorB;
instance IsFactor FactorC;

data SettingA = SA deriving (Show)
data SettingB = SB deriving (Show)
data SettingC = SC deriving (Show)


type family FactorSettings a :: *
type instance FactorSettings FactorA = SettingA
type instance FactorSettings FactorB = SettingB
type instance FactorSettings FactorC = SettingC

data FT = forall a. (Show (FactorSettings a), IsFactor a) => FT
  { factor :: a
  , settings :: FactorSettings a
  }

showFT :: FT -> String
showFT FT{..} = show settings

main = print $ showFT $ FT (undefined :: FactorA) SA
