{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
-- |

module Main where

import GHC.TypeLits
import Data.Proxy

newtype SuffixedString (s :: Symbol) = SS String

suffixed :: String -> SuffixedString s
suffixed s = SS s

toString :: forall suffix. KnownSymbol suffix => SuffixedString suffix -> String
toString (SS str) = str ++ "@" ++ symbolVal (Proxy :: Proxy suffix)

main :: IO ()
main = do
  let id1 = suffixed "simon" :: SuffixedString "teacher"
  let id2 = suffixed "simon" :: SuffixedString "dev"
  putStrLn (toString id1)
  putStrLn (toString id2)
