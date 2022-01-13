{-# LANGUAGE GADTs #-}
-- |

module Color where

import Text.Printf
import Data.Word

data RGB = RGB
  { rgbRed :: Word8
  , rgbGreen :: Word8
  , rgbBlue :: Word8
  } deriving (Eq, Show)


class IsColor a where
  toRGB :: a -> RGB

instance IsColor RGB where
  toRGB = id


data AliceBlue = AliceBlue

instance IsColor AliceBlue where
  toRGB _ = RGB 0xF0 0xF8 0xFF

toHex :: IsColor a => a -> String
toHex a = let (RGB r g b) = toRGB a
          in printf "%02x%02x%02x" r g b


-- Existential Type
data SomeColor = forall color. IsColor color => SomeColor color

instance Show SomeColor where
  show (SomeColor c) = show (toRGB c)

instance IsColor SomeColor where
  toRGB (SomeColor c) = toRGB c
