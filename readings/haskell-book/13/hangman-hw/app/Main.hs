module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Maybe (fromMaybe, isJust)
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Random (randomRIO)

type EnglishWord = String
-- newtype EnglishWord = EnglishWord String
type WordList = [EnglishWord]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

maxGuessChance :: Int
maxGuessChance = 7

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

isGameLengthWord :: EnglishWord -> Bool
isGameLengthWord str = length str >= minWordLength && length str < maxWordLength

gameWords :: WordList -> WordList
gameWords = filter isGameLengthWord

genGameWords :: IO WordList
genGameWords = fmap gameWords allWords

randomWord :: WordList -> IO EnglishWord
randomWord ws = do
  rIndex <- randomRIO (0, length ws - 1)
  return (ws !! rIndex)

genRandowWord :: IO EnglishWord
genRandowWord = genGameWords >>= randomWord

-- TODO: why Maybe??
type DiscoveredChar = Maybe Char

type GuessedChar = Char

data Puzzle = Puzzle EnglishWord [DiscoveredChar] [GuessedChar]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
      ++ " Guessed so far: "
      ++ guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar = fromMaybe '_'

freshPuzzle :: String -> Puzzle
freshPuzzle w = Puzzle w (take (length w) (repeat Nothing)) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = c `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = c `elem` guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar guessed) c =
  let zipper wordChar guessChar = if wordChar == c then Just wordChar else Nothing
      newFilledInSoFar = zipWith zipper word filledInSoFar
  in
    Puzzle word newFilledInSoFar (c : guessed)

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > maxGuessChance
    then do
      putStrLn "You lose!"
      putStrLn $ "The word was: " ++ wordToGuess
      exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle wordToGuess filledInSoFar _) =
  if all isJust filledInSoFar
    then do
      putStrLn "You win!"
      exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- genRandowWord
  runGame $ freshPuzzle (fmap toLower word)
