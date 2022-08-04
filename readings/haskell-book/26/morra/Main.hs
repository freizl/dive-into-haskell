module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.Environment
import System.IO
import System.Random
import Text.Read

type Score = Int

type PlayName = String

type Morra a = StateT [(PlayName, Score)] IO a

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
  a <- liftIO aiGuess
  b <- liftIO humanGuess
  modify (\s -> ("C", a) : ("P", b) : s)

gameHumanToHuman :: Morra ()
gameHumanToHuman = do
  (a, b) <- liftIO $ do
    x <- humanGuess
    clearScreen
    y <- humanGuess
    clearScreen
    return (x, y)
  modify (\s -> ("P1", a) : ("P2", b) : s)

main :: IO ()
main = do
  args <- getArgs
  putStrLn "=== Morra - Have Fun ==="
  let game = if null args then gameAiToHuman else gameHumanToHuman
  (_, [(p1, a), (p2, b)]) <- runStateT game []
  putStrLn (p1 ++ ": " ++ show a)
  putStrLn (p2 ++ ": " ++ show b)
  if even (a + b)
    then putStrLn (p1 ++ " - wins")
    else putStrLn (p2 ++ " - Player wins")
