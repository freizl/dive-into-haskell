{-# LANGUAGE OverloadedStrings #-}

module Main where

import Project

import Data.List

main :: IO ()
main = do
  sdb <- initSalesDB
  report1 sdb

report1 :: SalesDB -> IO ()
report1 = mapM_ (printR1. sumCost) . groupBy gCID . sortBy sCID
  where gCID s1 s2 = scust s1 == scust s2
        sumCost = foldr1 sum'
        sum' s1 s2 = s1 {samount = samount s1 + samount s2 }
        printR1 s = putStrLn $ show (scust s) ++ "   " ++ show (samount s)
