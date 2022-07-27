module StringProcess where

notThe :: String -> Maybe String
notThe xs = if xs == "the" then Nothing else Just xs

replaceThe :: String -> String
replaceThe = unwords . map (theToA . notThe) . words

theToA :: Maybe String -> String
theToA Nothing = "a"
theToA (Just x) = x

vowelChars :: [Char]
vowelChars = "aeiouAEIOU"

isVowelChar :: Char -> Bool
isVowelChar x = x `elem` vowelChars

-- Assume the string is in lower case
-- otherwise just need additional check
--
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel xs = go (words xs) 0

go :: [String] -> Integer -> Integer
go xss count
  | length xss <= 1 = count
  | otherwise =
      let (x : y : zs) = xss
       in if x == "the" && isVowelChar (head y) then go (y : zs) count + 1 else go (y : zs) count

countVowels :: String -> Integer
countVowels = toInteger . length . filter isVowelChar
