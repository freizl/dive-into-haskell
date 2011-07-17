{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Form where

import System.Random
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar

import Yesod.Form.Jquery
import Helloyesod

instance YesodJquery Helloyesod

data Params = Params
    { minNumber :: Int
    , maxNumber :: Int
    , singleWord :: Text
    , pluralWord :: Text
    , birthday :: Maybe Day
    }

--paramsFormlet :: Maybe Params -> Form s m Params
paramsFormlet mparams = fieldsToTable $ Params
    <$> intField "Minimum number" (fmap minNumber mparams)
    <*> intField "Maximum number" (fmap maxNumber mparams)
    <*> stringField "Single word" (fmap singleWord mparams)
    <*> stringField "Plural word" (fmap pluralWord mparams)
    <*> maybeJqueryDayField def "My birthday" (fmap birthday mparams)
    
-- | Demostrate form usage
getFormR :: Handler RepHtml
getFormR = do
    sess <- getSession
    (res, form, enctype) <- runFormGet $ paramsFormlet Nothing
    output <- case res of
                  FormMissing   -> return "Please fill out the form to get a result"
                  FormFailure _ -> return "Please correct the errors below"
                  FormSuccess (Params min max single plural bday) -> do
                    number <- liftIO $ randomRIO (min, max)
                    let word = if number == 1 then single else plural
                    return $ T.concat ["You got ", T.pack $ show number, " ",  word]
    defaultLayout $ do
    setTitle "Form Sandbox"
    addWidget $(widgetFile "form")
