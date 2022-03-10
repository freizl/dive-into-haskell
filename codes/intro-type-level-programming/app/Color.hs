{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
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


type ValidRGB r g b =
  ( KnownNat r, r <= 255
  , KnownNat g, g <= 255
  , KnownNat b, b <= 255)

-- | RGBTypeLevel and ValidRGB has to be use together.
-- Still not ideal.
data RGBTypeLevel (r :: Nat) (g :: Nat) (b :: Nat) = RGBTypeLevel
-- AllowAmbiguousTypes
-- ScopedTypeVariables
instance ValidRGB r g b => IsColor (RGBTypeLevel r g b) where
  toRGB _ = RGB (natWord8 @r) (natWord8 @g) (natWord8 @b)
    where
      natWord8 :: forall n. (KnownNat n, n <= 255) => Word8
      natWord8 = fromIntegral $ natVal (Proxy @n)

-- | Similar to RGBTypeLevel but given name
data NamedRGB (name :: Symbol) (r :: Nat) (g :: Nat) (b :: Nat) = NamedRGB
instance ValidRGB r g b => IsColor (NamedRGB name r g b) where
  toRGB _ = toRGB (RGBTypeLevel :: RGBTypeLevel r g b)


class IsColor a => NamedColor a where
  type ColorName a :: Symbol

instance NamedColor AliceBlue where
  type ColorName AliceBlue = "AliceBlue"

instance NamedColor Plum where
  type ColorName Plum = "Plum"

instance ValidRGB r g b => NamedColor (NamedRGB n r g b) where
  type ColorName _ = n



data RenameColor (name :: Symbol) =
  forall color. IsColor color => RenameColor color

instance IsColor (RenameColor name) where
  toRGB (RenameColor c) = toRGB c

instance NamedColor (RenameColor name) where
  type ColorName _ = name


colorNameVal :: forall a. KnownSymbol (ColorName a) => a -> String
colorNameVal _ = symbolVal $ Proxy @(ColorName a)


-- | Name RGB Color at type level
instance IsColor (RGBTypeLevel r g b) => NamedColor (RGBTypeLevel r g b) where
  type
    ColorName _ =
    "#" :++: PadNatHex r :++: PadNatHex g :++: PadNatHex b

infixr 6 :++:
type a :++: b = AppendSymbol a b

type family PadNatHex (n :: Nat) :: Symbol where
  PadNatHex n = IfThenElse (n <=? 15) ("0" :++: NatHex n) (NatHex n)

type family IfThenElse (p :: Bool) (t :: a) (f :: a) :: a where
  IfThenElse True t f = t
  IfThenElse False t f = f

type family NatHex (n :: Nat) :: Symbol where
  NatHex 0 = "0"
  NatHex 1 = "1"
  NatHex 2 = "2"
  NatHex 3 = "3"
  NatHex 4 = "4"
  NatHex 5 = "5"
  NatHex 6 = "6"
  NatHex 7 = "7"
  NatHex 8 = "8"
  NatHex 9 = "9"
  NatHex 10 = "A"
  NatHex 11 = "B"
  NatHex 12 = "C"
  NatHex 13 = "D"
  NatHex 14 = "E"
  NatHex 15 = "F"
  NatHex n = NatHex (Div n 16) :++: NatHex (Mod n 16)
