{-# LANGUAGE OverloadedStrings #-}

module Project where

import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Printf

sCID :: Sales -> Sales -> Ordering
sCID = comparing scust

type CustomerID = Int
type OrderID = Text

data Customer = Customer { cid      :: CustomerID
                         , cname    :: Text
                         , cage     :: Text
                         , cphone   :: Text
                         , caddress :: Text
                         } deriving (Show, Eq)

data Sales = Sales { sorder  :: OrderID
                   , scust   :: CustomerID
                   , sdate   :: Text
                   , samount :: Float
                   } deriving (Show, Eq)

type CustDB = Map.Map CustomerID Customer
type OrderDB = Map.Map OrderID [Text]
type SalesDB = [Sales]

initCustDB :: IO CustDB
initCustDB = do
  ls <- readDBFile "CustDB.txt"
  let cs = map init' ls
  return $ Map.fromList cs
  where init' (i:n:a:p:d:_) = (fromText i, Customer (fromText i) n a p d)

initOrderDB :: IO OrderDB
initOrderDB = do
  ls <- readDBFile "OrderDB.txt"
  let cs = map listToTuple ls
      gp = groupBy eqOrderId $ sort cs
      rs = map (foldr1 merge') gp
  return $ Map.fromList rs
  where listToTuple (i:n:_) = (i, [n])
        eqOrderId (i, _) (j, _) = i == j
        merge' (i, xs) (_, ys) = (i, xs++ys)

initSalesDB :: IO SalesDB
initSalesDB = do
  ls <- readDBFile "SalesDB.txt"
  let cs = map init' ls
  return cs
  where init' (o:c:d:total:_) = Sales o (fromText c) d (fromText total)

-- | Read DB Text file and split each line
--
readDBFile :: FilePath -> IO [[T.Text]]
readDBFile fp = fmap convert' (T.readFile fp)
                where convert' = map (T.splitOn "|" . T.strip) . T.lines

fromText :: Read a => Text -> a
fromText = read . T.unpack
