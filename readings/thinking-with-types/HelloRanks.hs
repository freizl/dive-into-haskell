{-# LANGUAGE RankNTypes #-}
-- |

module HelloRanks where

applyToFive :: (forall a. a -> a) -> Int
applyToFive f = f 5
