module Main () where

import JSON

main = print (JObject [("haskell", JString "JSON"), ("Hello", JNumber 2.3)])
