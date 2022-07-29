-- | 

module Palindrome where

import Control.Monad
import System.Exit (exitSuccess)
import Data.Char

-- in order to make it work for sentence like
-- "Madam I’m Adam"
-- has to remove space, maybe non-alpha char
-- goal/requirement is a bit ambiguity.
--
palindrome :: IO ()
palindrome = forever $ do
  line1 <- normalizeInput <$> getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nope!" >> exitSuccess

normalizeInput :: String -> String
normalizeInput = map toLower . filter isLetter
