{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Network.Wai (Response)
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config = Config
  { counts :: IORef (M.Map Text Integer),
    prefix :: Text
  }

type Scotty = ScottyT Text (ReaderT Config IO)

type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp ::
  Text ->
  M.Map Text Integer ->
  (M.Map Text Integer, Integer)
bumpBoomp k m = case M.lookup k m of
  Nothing -> (M.insert k 1 m, 1)
  Just i -> (M.insert k (i + 1) m, i + 1)

app :: Scotty ()
app = do
  get "/:key" $ do
    config <- lift ask
    unprefixed <- param "key"
    let key' = mappend (prefix config) unprefixed
    existingCounts <- liftIO $ readIORef (counts config)
    let (updatedCounts, newInteger) = bumpBoomp key' existingCounts
    liftIO $ writeIORef (counts config) updatedCounts
    html $
      mconcat
        [ "<h1>Success! Count was: ",
          TL.pack $ show newInteger,
          "</h1>"
        ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "please specify a prefix"
    (prefixArg : _) -> do
      counter <- newIORef M.empty
      let config = Config {counts = counter, prefix = TL.pack prefixArg}
      -- runR :: ReaderT Config IO Response -> IO Response
      let runR ma = runReaderT ma config
      scottyT 3000 runR app
