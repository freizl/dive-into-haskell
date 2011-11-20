module Main where

{- | Trivial Monad Transformer example

-}

import Data.Char
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

main :: IO ()
main = do 
    runMaybeT askPassword
    return ()
      

isValid :: String -> Bool
isValid s = length s >= 8 && any isAlpha s && any isNumber s && any isPunctuation s

getValidPassword :: MaybeT IO String
getValidPassword = do s <- lift getLine
                      guard (isValid s)
                      return s
 
askPassword :: MaybeT IO ()
askPassword = do lift $ putStrLn "Insert your new password:"
                 value <- getValidPassword
                 lift $ putStrLn $ "Storing in database..." ++ value
