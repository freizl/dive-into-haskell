-- | Demostrate a type that is both State and Reader.
--   In more general, combine Monad Transform

module Main where

import Control.Monad.State
import Control.Monad.Reader

type AState  = Int
type AReader = String

type App  = ReaderT AReader (StateT AState IO)
type App2 = StateT AState (ReaderT AReader IO)

initR :: AReader
initR = "Hello, ReaderT"

updateState :: AState -> AState
updateState = (+ 2)

-- | test case one. use type `App`
test :: App Int
test = do
       modify updateState 
       r <- ask
       liftIO $ print r
       s <- get
       return s

-- | test case two. use type `App2`
test2 :: App2 Int
test2 = modify updateState 
        >> ask >>= liftIO . print 
        >> modify updateState 
        >> get >>= return

main :: IO ()
main =    (evalStateT (runReaderT test initR) 0)  >>= print
       >> (runReaderT (evalStateT test2 0) initR) >>= print
       
