module CodeGen where

import Name
import StgSyn
import Var
import qualified Data.ByteString.Char8 as C


fooCompile :: StgTopBinding -> String
fooCompile (StgTopLifted gsb) = fooCompile2 gsb
fooCompile (StgTopStringLit bndr bs) = C.unpack bs ++ nameToString bndr

fooCompile2 :: GenStgBinding Id Id -> String
fooCompile2 (StgNonRec bndr rhs) = "stg-non-rec:" ++ nameToString bndr
fooCompile2 (StgRec xs) = "stg-rec xs"

nameToString = nameStableString . getName
