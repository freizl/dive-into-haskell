{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Reader
import Control.Monad.State
import Data.Text
import qualified Data.Map.Strict as M
import Prelude hiding (unwords)

--type ContextName = Text

{- Reader Monad, works -}
context :: ReaderT Text IO Text
context = do
    name <- ask
    return name

main1 :: IO ()
main1 = do
    name <- runReaderT context "CONCRETE_CONTEXT"
    print (name)

{- State Monad, works -}
type AppState = M.Map Text Text

addValue :: Text -> Text -> StateT AppState IO ()
addValue k v = StateT $ \st ->
    return ( (), M.insert k v st)

getValue :: Text -> StateT AppState IO (Maybe Text)
getValue k = StateT $ \st ->
    return (val st, st)
    where
        val st = M.lookup k st

modifyValue :: Text -> Text -> StateT AppState IO ()
modifyValue k v = StateT $ \st ->
    return ( (), M.insert k v (M.delete k st) )

runWithState :: StateT AppState IO (Maybe Text)
runWithState = do
    addValue "k1" "v1"
    addValue "k2" "v2"
    modifyValue "k2" "v_2"
    getValue "k2"

main2 :: IO ()
main2 = do
    st <- runStateT runWithState M.empty
    print (fst st)
    print (snd st)

{- Use reader monad in state monad, works-}
addValueWithContext :: Text -> Text -> StateT AppState (ReaderT Text IO) ()
addValueWithContext k v = StateT $ \st -> do
    ctx <- ask
    return ( (), M.insert k (concate ctx v) st)
    where
        concate ctx val = unwords [val, "in", ctx]

getValueInContext :: Text -> StateT AppState (ReaderT Text IO) (Maybe Text)
getValueInContext k = StateT $ \st ->
    return (val st, st)
    where
        val st = M.lookup k st

runStateWithContext :: StateT AppState (ReaderT Text IO) (Maybe Text)
runStateWithContext = do
    addValueWithContext "k1" "v1"
    addValueWithContext "k2" "v2"
    getValueInContext "k2"

main3 :: IO ()
main3 = do
    st <- runReaderT (runStateT runStateWithContext M.empty) "CONCRETE_CONTEXT"
    print (fst st)
    print (snd st)

{- Use state monad in reader monad, FAILED! -}
addValueWithContext2 :: Text -> Text -> ReaderT Text (StateT AppState IO) ()
addValueWithContext2 k v = do
    ctx <- ask
    st <- get
    put $ M.insert k (concate ctx v) st
    where
      concate ctx val = unwords [val, "in", ctx]

runStateWithContext2 :: ReaderT Text (StateT AppState IO) ()
runStateWithContext2 = do
    addValueWithContext2 "k1" "v1"
    addValueWithContext2 "k2" "v2"

main4 :: IO ()
main4 = do
    s <- runStateT (runReaderT runStateWithContext2 "CONCRETE_CONTEXT") M.empty
    print (snd s)

main :: IO ()
main = do
  putStrLn "1111"
  main1
  putStrLn "2222"
  main2
  putStrLn "3333"
  main3
  putStrLn "444"
  main4
