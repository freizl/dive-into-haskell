{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Helloyesod.hs file.
module Settings
    ( hamletFile
    , cassiusFile
    , juliusFile
    , luciusFile
    , widgetFile
    , connStr
    , ConnectionPool
    , withConnectionPool
    , runConnectionPool
    , approot
    , staticroot
    , staticdir
    ) where

import qualified Text.Hamlet as H
import qualified Text.Cassius as H
import qualified Text.Julius as H
import qualified Text.Lucius as H
import Language.Haskell.TH.Syntax
import Database.Persist.Sqlite

import Yesod (MonadControlIO, addWidget, addCassius, addJulius, addLucius)
import Data.Monoid (mempty, mappend)
import System.Directory (doesFileExist)
import Data.Text (Text)

-- | The base URL for your application. This will usually be different for
-- development and production. Yesod automatically constructs URLs for you,
-- so this value must be accurate to create valid links.
-- Please note that there is no trailing slash.
approot :: Text
approot =
#ifdef PRODUCTION
-- You probably want to change this. If your domain name was "yesod.com",
-- you would probably want it to be:
-- > "http://yesod.com"
  "http://localhost:3000"
#else
  "http://localhost:3000"
#endif

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticdir :: FilePath
staticdir = "static"

-- | The base URL for your static files. As you can see by the default
-- value, this can simply be "static" appended to your application root.
-- A powerful optimization can be serving static files from a separate
-- domain name. This allows you to use a web server optimized for static
-- files, more easily set expires and cache values, and avoid possibly
-- costly transference of cookies on static files. For more information,
-- please see:
--   http://code.google.com/speed/page-speed/docs/request.html#ServeFromCookielessDomain
--
-- If you change the resource pattern for StaticR in Helloyesod.hs, you will
-- have to make a corresponding change here.
--
-- To see how this value is used, see urlRenderOverride in Helloyesod.hs
staticroot :: Text
staticroot = approot `mappend` "/static"

-- | The database connection string. The meaning of this string is backend-
-- specific.
connStr :: Text
connStr =
#ifdef PRODUCTION
  "production.db3"
#else
  "debug.db3"
#endif

-- | Your application will keep a connection pool and take connections from
-- there as necessary instead of continually creating new connections. This
-- value gives the maximum number of connections to be open at a given time.
-- If your application requests a connection when all connections are in
-- use, that request will fail. Try to choose a number that will work well
-- with the system resources available to you while providing enough
-- connections for your expected load.
--
-- Also, connections are returned to the pool as quickly as possible by
-- Yesod to avoid resource exhaustion. A connection is only considered in
-- use while within a call to runDB.
connectionCount :: Int
connectionCount = 10

-- The rest of this file contains settings which rarely need changing by a
-- user.

-- The following three functions are used for calling HTML, CSS and
-- Javascript templates from your Haskell code. During development,
-- the "Debug" versions of these functions are used so that changes to
-- the templates are immediately reflected in an already running
-- application. When making a production compile, the non-debug version
-- is used for increased performance.
--
-- You can see an example of how to call these functions in Handler/Root.hs
--
-- Note: due to polymorphic Hamlet templates, hamletFileDebug is no longer
-- used; to get the same auto-loading effect, it is recommended that you
-- use the devel server.

-- | expects a root folder for each type, e.g: hamlet/ lucius/ julius/
globFile :: String -> String -> FilePath
globFile kind x = kind ++ "/" ++ x ++ "." ++ kind

hamletFile :: FilePath -> Q Exp
hamletFile = H.hamletFile . globFile "hamlet"

cassiusFile :: FilePath -> Q Exp
cassiusFile = 
#ifdef PRODUCTION
  H.cassiusFile . globFile "cassius"
#else
  H.cassiusFileDebug . globFile "cassius"
#endif

luciusFile :: FilePath -> Q Exp
luciusFile = 
#ifdef PRODUCTION
  H.luciusFile . globFile "lucius"
#else
  H.luciusFileDebug . globFile "lucius"
#endif

juliusFile :: FilePath -> Q Exp
juliusFile =
#ifdef PRODUCTION
  H.juliusFile . globFile "julius"
#else
  H.juliusFileDebug . globFile "julius"
#endif

widgetFile :: FilePath -> Q Exp
widgetFile x = do
    let h = unlessExists (globFile "hamlet")  hamletFile
    let c = unlessExists (globFile "cassius") cassiusFile
    let j = unlessExists (globFile "julius")  juliusFile
    let l = unlessExists (globFile "lucius")  luciusFile
    [|addWidget $h >> addCassius $c >> addJulius $j >> addLucius $l|]
  where
    unlessExists tofn f = do
        e <- qRunIO $ doesFileExist $ tofn x
        if e then f x else [|mempty|]

-- The next two functions are for allocating a connection pool and running
-- database actions using a pool, respectively. It is used internally
-- by the scaffolded application, and therefore you will rarely need to use
-- them yourself.
withConnectionPool :: MonadControlIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withSqlitePool connStr connectionCount

runConnectionPool :: MonadControlIO m => SqlPersist m a -> ConnectionPool -> m a
runConnectionPool = runSqlPool