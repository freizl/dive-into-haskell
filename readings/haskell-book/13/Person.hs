module Person where

import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Text.Read

type Name = String

type Age = Integer

data Person = Person Name Age deriving (Show)

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson ::
  Name ->
  Age ->
  Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $
        PersonInvalidUnknown $
          "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr "What is your name: "
  name <- getLine
  putStr "What is your age: "
  ageStr <- getLine
  case readMaybe ageStr :: Maybe Age of
    Just age -> case mkPerson name age of
      Right p -> putStrLn $ "Yay! Successfully got a person: " ++ show p
      Left e -> print e
    Nothing -> putStrLn $ "Unable to parse age: " ++ ageStr

main :: IO ()
main = gimmePerson
