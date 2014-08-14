{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import Data.List
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Project

main :: IO ()
main = do
  cdb <- initCustDB
  odb <- initOrderDB
  sdb <- initSalesDB
  report2 cdb odb sdb

report2 :: CustDB -> OrderDB -> SalesDB -> IO ()
report2 cdb odb sdb = mapM_ printR2 $ filter (greatThan60 cdb) $ sortBy sCID sdb
  where getCName cid' = case Map.lookup cid' cdb of
                          Just c -> cname c
                          Nothing -> ""
        getOItems oid' = fromMaybe [] (Map.lookup oid' odb)
        printR2 sales = T.putStrLn $ (getCName $ scust sales) `T.append` "   " `T.append`
                                sorder sales `T.append` "   " `T.append`
                                sdate sales `T.append` "   " `T.append`
                                (T.intercalate "," $ getOItems $ sorder sales)


greatThan60 :: CustDB -> Sales -> Bool
greatThan60 cdb sales = case Map.lookup (scust sales) cdb of
                          Just c -> cage c >= 60
                          Nothing -> False
