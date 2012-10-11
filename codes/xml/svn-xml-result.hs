{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Main where

import System.Process
import Control.Arrow
import Text.XML.HXT.Core

main :: IO ()
main = do
  repoUrl   <- return "http://svn/repos/thirdparty/"
  xmlResult <- readProcess "svn" ["ls", "--xml", repoUrl ] ""
  nodes     <- runX (readString [withValidate no] xmlResult
                     >>> getSvnNode)
  print nodes


data SvnNode = SN { name, author, date :: String } deriving (Show)

atTag tag = deep (isElem >>> hasName tag)
text      = getChildren >>> getText

getSvnNode = atTag "entry" >>>                             
  proc x -> do
    nameInp   <- text <<< atTag "name"   -< x
    authorInp <- text <<< atTag "author" -< x
    dateInp   <- text <<< atTag "date"   -< x
    returnA  -< SN { name = nameInp, author = authorInp, date = dateInp }
