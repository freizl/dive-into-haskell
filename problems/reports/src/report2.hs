{-# LANGUAGE OverloadedStrings #-}

module Main where

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
