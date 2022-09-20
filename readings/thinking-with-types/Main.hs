module Main where

import Data.List
import Data.Maybe
import Data.Typeable
import HelloBuildInTypes

main :: IO ()
main = pure ()

-- AllowAmbiguousTypes
-- typeName :: forall a. Typeable a => String -- ! 1
-- typeName = show . typeRep $ Proxy @a  -- ! 2

f :: String -> [(String, Int)] -> [(String, Int)]
f day xs = reverse $ takeWhile ((>=) day . fst) xs

