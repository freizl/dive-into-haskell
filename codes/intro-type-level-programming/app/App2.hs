{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Data.Map.Strict as Map
import GHC.TypeLits -- GHC primitives type-level Kinds (Nat, Symbol, Char)
import Data.Proxy
import Data.Kind

import Color

-- | without DataKinds, this only creates a Type alias
-- otherwise, it also creates Kind alias
type Theme = [Symbol]

newtype ThemeInstance ( theme :: Theme ) = ThemeInstance
  { getThemeInstance :: Map.Map String SomeColor }
  deriving Show


class HasColor (color :: Symbol) (theme :: Theme)
-- instance (color ~ current) => HasColor color (current : rest)
-- | Base case
-- equivalent to above
--
instance HasColor color (color : rest)
-- | Recursive case
-- > If the tail of the theme list has the color you are looking for,
-- > then the entire list has the color you are looking for
--
-- Why OVERLAPPABLE?
-- 'color' and 'color\'' don’t have to be the same, but they might be.
-- Therefore both ~HasColor~ instance could match in some cases.
-- Hence we have to tell compile which instance to choose.
instance {-# OVERLAPPABLE #-}
  HasColor color rest => HasColor color (color' : rest)


lookupColor :: forall colorName theme.
  ( KnownSymbol colorName
  , HasColor colorName theme)
  => ThemeInstance theme -> RGB
lookupColor (ThemeInstance colors) =
  let targetName = symbolVal (Proxy @colorName)
  in toRGB (colors Map.! targetName)

testFunc :: HasColor color theme => ()
testFunc = ()

type DemoTheme = '["red", "blue", "green"]

myThemeWithPitfall :: ThemeInstance DemoTheme
myThemeWithPitfall =
  -- TODO: why compiler doesn't complain as the list doesn't
  -- have all required theme (defined by type DemoTheme)?
  -- See section: Constructing A Theme Instance At The Type Level
  ThemeInstance @DemoTheme $
    Map.fromList
      [ ("foreground", SomeColor $ RGB 0x3a 0x20 0x35),
        ("backgronud", SomeColor $ RGB 0xdd 0xa0 0xdd),
        ("red", SomeColor $ RGB 255 0 0),
        ("blue", SomeColor AliceBlue)
      ]

data MakeSimpleTheme (theme :: Theme) where
  SimpleNewTheme :: MakeSimpleTheme '[]
  SimpleAddColor :: (KnownSymbol colorName) => SomeColor -> MakeSimpleTheme theme -> MakeSimpleTheme (colorName: theme)

-- | TODO: the order of color matters because of `DemoTheme` type array
-- Is there way we can relax the constrait: as long as it has three color defined
-- in `DemoTheme`, it's fine.
--
simpleThemeDemo :: MakeSimpleTheme DemoTheme
simpleThemeDemo =
  SimpleAddColor @"red" (SomeColor $ RGB 255 0 0) $
  SimpleAddColor @"blue" (SomeColor $ RGB 0 0 255) $
  SimpleAddColor @"green" (SomeColor $ RGB 0 255 0) $
  SimpleNewTheme

class ToThemeInstance (a :: Theme -> Type) (t :: Theme) where
  toThemeInstance :: a t -> ThemeInstance t

instance ToThemeInstance MakeSimpleTheme '[] where
  toThemeInstance _ = ThemeInstance Map.empty

instance (ToThemeInstance MakeSimpleTheme rest) => ToThemeInstance MakeSimpleTheme (color: rest) where
  toThemeInstance (SimpleAddColor sc mst) =
    let (ThemeInstance colors) = toThemeInstance mst
        targetName = symbolVal (Proxy @color)
    in ThemeInstance (Map.insert targetName sc colors)

myTheme :: ThemeInstance DemoTheme
myTheme = toThemeInstance simpleThemeDemo

data PolybarColorScheme = PolybarColorScheme
  { focusedWorkspaceText :: RGB,
    focusedWorkspaceBackground :: RGB
  }
  deriving (Show)

polybarColorScheme :: ThemeInstance DemoTheme -> PolybarColorScheme
polybarColorScheme theme =
  PolybarColorScheme
    (lookupColor @"red" theme)
    (lookupColor @"blue" theme)

main :: IO ()
main = do
  -- compilation error
  -- return $ testFunc @"red" @'["blue"]
  return $ testFunc @"red" @'["blue", "red"]
  print (polybarColorScheme myTheme)
