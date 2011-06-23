-- http://www.haskell.org/haskellwiki/HXT/Practical/Simple1

{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
import Text.XML.HXT.Core

data Guest = Guest { firstName, lastName :: String }
           deriving (Show, Eq)

main = do
  guests <- runX (readDocument [withValidate no] "simple1.xml"
                  >>> getGuest2)
  print guests

getGuest2 = atTag "guest" >>>
  proc x -> do
    fname <- text <<< atTag "fname" -< x
    lname <- text <<< atTag "lname" -< x
    returnA -< Guest { firstName = fname, lastName = lname }

atTag tag = deep (isElem >>> hasName tag)
text = getChildren >>> getText
