{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import Test.QuickCheck

main :: IO ()
main = print $ cyclicAutomorphisms $ longStrGen "byebye"

cyclicAutomorphisms :: B.ByteString -> Int
cyclicAutomorphisms xs = length$ filter (== xs) $ allPossibles xs

allPossibles :: B.ByteString -> [B.ByteString]
allPossibles xs = [ cyclicShift x xs | x <- [1..len] ]
  where len = B.length xs

cyclicShift :: Int -> B.ByteString -> B.ByteString
cyclicShift n xs = conss $ B.splitAt n xs
  where conss (x, y) = B.append y x

-- | Tests
instance Arbitrary B.ByteString where
  arbitrary   = fmap B.pack arbitrary        

prop_cyclicShitf xs = cyclicShift 0 xs == xs

prop_cyclicAutomorphisms xs = cyclicAutomorphisms xs == cyclicAutomorphisms xs

-- VERY SLOW
prop_cyclicAutomorphisms_long xs 
  | B.null xs  = True
  | otherwise  = let ys = longStrGen xs in
                 cyclicAutomorphisms ys == cyclicAutomorphisms ys
  
-- | Utils                 
longStrGen :: B.ByteString -> B.ByteString
longStrGen = B.pack . replicate 30000 . B.head
