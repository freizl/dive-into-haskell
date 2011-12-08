{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Contact where

import Helloyesod

getContactR :: Handler RepHtml
getContactR = do
    defaultLayout $ do
    setTitle "Contact"
    addWidget $(widgetFile "contact")
