module Main where

import System.IO
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.Environment (getArgs)
import Text.Read
import System.Random

type Score = Int
type Morra a = StateT [ Score ] IO a


aiGuess :: IO Int
aiGuess = randomRIO (1, 2)


getLineWithoutEcho = do
  hSetEcho stdin False
  str <- getLine
  hSetEcho stdin True
  putChar '\n'
  return str


humanGuess :: IO Int
humanGuess = do
  hSetBuffering stdout NoBuffering
  putStr "Guess your number: "
  str <- getLineWithoutEcho
  -- TODO: shall we constrain to number 1 or 2.?
  case readEither str of
    Right i -> return i
    Left e -> putStrLn e >> putStrLn "Not a number, please try again" >> humanGuess

game :: Morra ()
game = do
  a <- liftIO aiGuess
  b <- liftIO humanGuess
  modify (\s -> a:b:s)

main :: IO ()
main = do
  putStrLn "=== Morra - Have Fun ==="
  (_, [a,b]) <- runStateT game []
  putStrLn ("C: " ++ show a)
  putStrLn ("P: " ++ show b)
  if even (a + b) then
    putStrLn "- Computer wins"
  else
    putStrLn "- Player wins"
