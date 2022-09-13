-- |

module Main where

import HelloBuildInTypes
import Data.Typeable

main :: IO ()
main = pure ()

-- typeName :: forall a. Typeable a => String -- ! 1
-- typeName = show . typeRep $ Proxy @a  -- ! 2
