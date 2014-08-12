{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Printf

main :: IO ()
main = do
  cdb <- initCustDB
  odb <- initOrderDB
  sdb <- initSalesDB
  report1 sdb
  report2 cdb odb sdb

report1 :: SalesDB -> IO ()
report1 = mapM_ (printR1. sumCost) . groupBy gCID . sortBy sCID
  where gCID s1 s2 = scust s1 == scust s2
        sumCost = foldr1 sum'
        sum' s1 s2 = s1 {samount = samount s1 + samount s2 }
        printR1 s = putStrLn $ show (scust s) ++ "   " ++ show (samount s)

sCID :: Sales -> Sales -> Ordering
sCID = comparing scust

report2 :: CustDB -> OrderDB -> SalesDB -> IO ()
report2 cdb odb sdb = do
  mapM_ printR2 $ sortBy sCID sdb
  where getCName cid' = case Map.lookup cid' cdb of
                          Just c -> cname c
                          Nothing -> ""
        getOItems oid' = case Map.lookup oid' odb of
                          Just c -> c
                          Nothing -> []
        printR2 sales = T.putStrLn $ (getCName $ scust sales) `T.append` "   " `T.append`
                                sorder sales `T.append` "   " `T.append`
                                sdate sales `T.append` "   " `T.append`
                                (T.intercalate "," $ getOItems $ sorder sales)

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
  ls <- fmap T.lines (T.readFile "CustDB.txt")
  let cs = map (init' . T.splitOn "|" . T.strip) ls
  return $ Map.fromList cs
  where init' (i:n:a:p:d:_) = (cid', Customer cid' n a p d)
                              where cid' = fromText i

initOrderDB :: IO OrderDB
initOrderDB = do
  ls <- fmap T.lines (T.readFile "OrderDB.txt")
  let cs = map (listToTuple . T.splitOn "|" . T.strip) ls
      gp = groupBy eqOrderId $ sort cs
      rs = map (foldr1 merge') gp
  return $ Map.fromList rs
  where listToTuple (i:n:_) = (i, [n])
        eqOrderId (i, _) (j, _) = i == j
        merge' (i, xs) (_, ys) = (i, xs++ys)

initSalesDB :: IO SalesDB
initSalesDB = do
  ls <- fmap T.lines (T.readFile "SalesDB.txt")
  let cs = map (init' . T.splitOn "|" . T.strip) ls
  return cs
  where init' (o:c:d:total:_) = Sales o (fromText c) d (fromText total)

fromText :: Read a => Text -> a
fromText = read . T.unpack
