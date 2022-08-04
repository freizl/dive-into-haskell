module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.Environment
import System.IO
import System.Random
import Text.Read (readEither)

type Score = Int

type PlayName = String

data AppState = AppState
  { scores :: [(PlayName, Score)],
    humanBehavior :: [Int]
  }

type Morra a = StateT AppState IO a

desiredScore :: (Int, Int)
desiredScore = (1, 2)

aiGuess :: IO Int
aiGuess = randomRIO desiredScore

getLineWithoutEcho = do
  hSetEcho stdin False
  str <- getLine
  hSetEcho stdin True
  putChar '\n'
  return str

-- https://stackoverflow.com/a/9359947
-- maybe try https://hackage.haskell.org/package/terminfo
--
clearScreen :: IO ()
clearScreen = putStr "\ESC[1J"

humanGuess :: IO Int
humanGuess = do
  hSetBuffering stdout NoBuffering
  putStr "Guess your number: "
  str <- getLineWithoutEcho
  case readEither str of
    Right i ->
      if fst desiredScore == i || snd desiredScore == i
        then return i
        else
          putStrLn "Not desired number (1,2). Try again"
            >> humanGuess
    Left e ->
      putStr e
        >> putStrLn ". Not a number, please try again"
        >> humanGuess

gameAiToHuman :: Morra ()
gameAiToHuman = do
  s1 <- get
  let hs = humanBehavior s1
  a <- if length hs == 3
    then liftIO aiGuess -- FIXME: I'm not fully understand the 3-gram
    else liftIO aiGuess
  b <- liftIO humanGuess
  let s2 = if length hs < 3 then s1 {humanBehavior = hs ++ [b]} else s1
  let s3 = s2 {scores = [("C", a), ("P", b)]}
  put s3

gameHumanToHuman :: Morra ()
gameHumanToHuman = do
  (a, b) <- liftIO $ do
    x <- humanGuess
    clearScreen
    y <- humanGuess
    clearScreen
    return (x, y)
  modify (\s -> s {scores = [("P1", a), ("P2", b)]})

runGame :: AppState -> Morra () -> IO AppState
runGame appState game = do
  (_, s) <- runStateT game appState
  let [(p1, a), (p2, b)] = scores s
  putStrLn (p1 ++ ": " ++ show a)
  putStrLn (p2 ++ ": " ++ show b)
  if even (a + b)
    then putStrLn (p1 ++ " - wins")
    else putStrLn (p2 ++ " - Player wins")
  runGame s game

main :: IO ()
main = do
  args <- getArgs
  putStrLn "=== Morra - Have Fun ==="
  let defaultAppState = AppState {scores = [], humanBehavior = []}
  let game = if null args then gameAiToHuman else gameHumanToHuman
  void $ runGame defaultAppState game
