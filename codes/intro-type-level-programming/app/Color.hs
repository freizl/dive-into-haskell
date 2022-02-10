{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Color where

import Text.Printf
import Data.Word
import GHC.TypeLits -- GHC primitives type-level Kinds (Nat, Symbol, Char)
import Data.Proxy
import Data.Kind

data RGB = RGB
  { rgbRed :: Word8
  , rgbGreen :: Word8
  , rgbBlue :: Word8
  } deriving (Eq, Show)


class IsColor a where
  toRGB :: a -> RGB

instance IsColor RGB where
  toRGB = id

toHex :: IsColor a => a -> String
toHex a = let (RGB r g b) = toRGB a
          in printf "%02x%02x%02x" r g b


-- | Some X11 Colors

data AliceBlue = AliceBlue

instance IsColor AliceBlue where
  toRGB _ = RGB 0xF0 0xF8 0xFF

data Plum = Plum
instance IsColor Plum where
  toRGB = const $ RGB 0xDD 0xA0 0xDD


-- | SomeColor is Existential Type
data SomeColor = forall color. IsColor color => SomeColor color

instance Show SomeColor where
  show (SomeColor c) = show (toRGB c)

instance IsColor SomeColor where
  toRGB (SomeColor c) = toRGB c


-- | RGBTypeLevel and ValidRGB has to be use together.
-- Still not ideal.
data RGBTypeLevel (r :: Nat) (g :: Nat) (b :: Nat) = RGBTypeLevel
type ValidRGB r g b =
  ( KnownNat r, r <= 255
  , KnownNat g, g <= 255
  , KnownNat b, b <= 255)

-- AllowAmbiguousTypes
-- ScopedTypeVariables
instance ValidRGB r g b => IsColor (RGBTypeLevel r g b) where
  toRGB _ = RGB (natWord8 @r) (natWord8 @g) (natWord8 @b)
    where
      natWord8 :: forall n. (KnownNat n, n <= 255) => Word8
      natWord8 = fromIntegral $ natVal (Proxy @n)
