{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}

module Controller
    ( withHelloyesod
    , withDevelApp
    ) where

import Helloyesod
import Settings
import Yesod.Helpers.Static
import Yesod.Helpers.Auth
import Database.Persist.GenericSql
import Data.ByteString (ByteString)
import Data.Dynamic (Dynamic, toDyn)

-- Import all relevant handler modules here.
import Handler.Root
import Handler.Contact

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Helloyesod.hs. Please see
-- the comments there for more details.
mkYesodDispatch "Helloyesod" resourcesHelloyesod

-- Some default handlers that ship with the Yesod site template. You will
-- very rarely need to modify this.
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "config/favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: ByteString)

getHelloR :: Handler RepHtml
getHelloR = defaultLayout $ do
  setTitle "Yesod Sandbox"
  addHtml   [$hamlet|<p>message from inner template, addHtml!|]
  addHamlet [$hamlet|<p>message from inner template, addHamlet!|]
  addWidget [$hamlet|<p>message from inner template, addWidget!|]

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withHelloyesod :: (Application -> IO a) -> IO a
withHelloyesod f = Settings.withConnectionPool $ \p -> do
    runConnectionPool (runMigration migrateAll) p
    let h = Helloyesod s p
    toWaiApp h >>= f
  where
    s = static Settings.staticdir

withDevelApp :: Dynamic
withDevelApp = toDyn (withHelloyesod :: (Application -> IO ()) -> IO ())
