module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.List (intersperse, nub)
import Data.Maybe (fromMaybe, isJust)
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Random (randomRIO)

-- newtype EnglishWord = EnglishWord String
-- newtype WordList = WordList [String]
--
type EnglishWord = String

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

-- | why Maybe?
-- in order to tell whether a particular position has been guessed correctly.
-- see @freshPuzzle@ for how it is initialized.
type DiscoveredChar = Maybe Char

type GuessedChar = Char

data Puzzle = Puzzle EnglishWord [DiscoveredChar] [GuessedChar]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
      ++ " Guessed so far: "
      ++ (intersperse ',' guessed)

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar = fromMaybe '_'

freshPuzzle :: String -> Puzzle
freshPuzzle w =
  Puzzle
    (fmap toLower w)
    (take (length w) (repeat Nothing))
    []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = c `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = c `elem` guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar guessed) c =
  let zipper wordChar guessChar = if wordChar == c then Just wordChar else guessChar
      newFilledInSoFar = zipWith zipper word filledInSoFar
   in Puzzle word newFilledInSoFar (guessed ++ [c])

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
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
gameOver (Puzzle wordToGuess filledSoFar guessed) =
  let validGuessCount = length $ nub $ filter isJust filledSoFar
      invalidGuessCount = length guessed - validGuessCount
   in if invalidGuessCount >= maxGuessChance
        then do
          putStrLn "You lose!"
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
  gameWin puzzle
  gameOver puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- genRandowWord
  print word
  runGame $ freshPuzzle word
