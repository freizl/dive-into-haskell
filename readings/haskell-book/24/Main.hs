module Main where

import Control.Applicative
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

two :: Parser Char
two = char '2'

p123 :: Parser String
p123 = string "123" <|> string "12" <|> string "1"

-- | another impl
q123 :: Parser String
q123 = many (oneOf "123")

mystring :: String -> Parser String
mystring = mapM char

main :: IO ()
main = do
  parseTest one "123"
  parseTest (one >> eof) "123"
  parseTest (one >> stop :: Parser Char) "123"
  parseTest (one >> two) "123"
  parseTest (one >> two >> eof) "123"
  parseTest p123 "1"
  parseTest p123 "12"
  parseTest p123 "123"
