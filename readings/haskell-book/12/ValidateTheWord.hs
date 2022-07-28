module ValidateTheWord where

newtype MyWord = MyWord String deriving (Eq, Show)

vowels :: [Char]
vowels = "aeiou"

isVowels :: Char -> Bool
isVowels c = c `elem` vowels

mkWord :: String -> Maybe MyWord
mkWord xs =
  let vs = filter isVowels xs
      len = length xs
      lenV = length vs
      lenC = len - lenV
   in if lenV > lenC then Nothing else Just (MyWord xs)
