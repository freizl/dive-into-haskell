module Main where

import Color
import qualified Data.Map.Strict as Map

newtype ThemeInstance1 = ThemeInstance1 {getThemeInstance1 :: Map.Map String RGB}
newtype ThemeInstance2 = ThemeInstance2 {getThemeInstance2 :: Map.Map String SomeColor}

myTheme1 :: ThemeInstance1
myTheme1 =
  ThemeInstance1 $
    Map.fromList
      [ ("foreground", RGB 0x3a 0x20 0x35),
        ("backgronud", RGB 0xdd 0xa0 0xdd)
      ]

myTheme2 :: ThemeInstance2
myTheme2 =
  ThemeInstance2 $
    Map.fromList
      [ ("foreground", SomeColor $ RGB 0x3a 0x20 0x35)
        , ("backgronud", SomeColor $ RGB 0xdd 0xa0 0xdd)
        , ("red", SomeColor $ RGB 255 0 0)
        , ("blue", SomeColor AliceBlue)
      ]


data PolybarColorScheme = PolybarColorScheme
  { focusedWorkspaceText :: RGB,
    focusedWorkspaceBackground :: RGB
  }
  deriving (Show)

polybarColorScheme :: PolybarColorScheme
polybarColorScheme =
  PolybarColorScheme
    { focusedWorkspaceText = RGB 0xdd 0xa0 0xdd,
      focusedWorkspaceBackground = RGB 0x2a 0x20 0x35
    }

polybarColorScheme1 :: ThemeInstance1 -> Maybe PolybarColorScheme
polybarColorScheme1 (ThemeInstance1 theme) = PolybarColorScheme
  <$> Map.lookup "foreground" theme
  <*> Map.lookup "foreground" theme

data PolybarColorScheme2 = PolybarColorScheme2
  { focusedWorkspaceText2 :: SomeColor,
    focusedWorkspaceBackground2 :: SomeColor
  }
  deriving (Show)

polybarColorScheme2 :: ThemeInstance2 -> Maybe PolybarColorScheme2
polybarColorScheme2 (ThemeInstance2 theme) = PolybarColorScheme2
  <$> Map.lookup "foreground" theme
  <*> Map.lookup "foreground" theme


main :: IO ()
main = do
  print polybarColorScheme
