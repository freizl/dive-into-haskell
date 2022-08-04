-- | Fix the code

module Exe2 where

import Control.Monad.Trans.Maybe
import Control.Monad

{-
  newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
-}

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = MaybeT $ do
  v <- getLine
  return $ if isValid v then Just v else Nothing
  -- guard $ isValid v
  -- return (Just v)

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e ->
      putStrLn ("Good, was very excite: " ++ e)

main :: IO ()
main = doExcite
