module Main where

import Data.List (findIndices)
import Data.Bits
import Control.Monad (msum)

-- | Play 'X' or 'O'.
--   TODO: shall define new data type `TX | TO`
type Item = Char
                     
-- | The Board Data Structure
type Board = String

-- | Binary representation. Size 18 because two bits for each slot.
type BoardBinRep = Int
-- | A possible solution in binary format
type SolutionBin = Int

-- | Transform Board to its binary representation
toBoardBinRep :: Board -> BoardBinRep
toBoardBinRep = binToDec . concatMap itemToBin

-- | 
binToDec :: [Int] -> Int
binToDec l = sum $ map (2^) $ findIndices (==1) $ reverse l

itemToBin :: Char -> [Int]
itemToBin x
  | x == 'X'  = [0,1]
  | x == 'O'  = [1,0]
  | otherwise = [0,0]

-- | easy to write 0/1 for representation
winnersTmp :: [[Int]]
winnersTmp = [ [0,1,0,0,0,0, 0,1,0,0,0,0, 0,1,0,0,0,0]
             , [0,0,0,1,0,0, 0,0,0,1,0,0, 0,0,0,1,0,0]
             , [0,0,0,0,0,1, 0,0,0,0,0,1, 0,0,0,0,0,1]               
             , [0,1,0,1,0,1, 0,0,0,0,0,0, 0,0,0,0,0,0]               
             , [0,0,0,0,0,0, 0,1,0,1,0,1, 0,0,0,0,0,0]
             , [0,0,0,0,0,0, 0,0,0,0,0,0, 0,1,0,1,0,1]
             , [0,1,0,0,0,0, 0,0,0,1,0,0, 0,0,0,0,0,1]
             , [0,0,0,0,0,1, 0,0,0,1,0,0, 0,1,0,0,0,0]
             ]
          
winners :: [SolutionBin]
winners = map binToDec winnersTmp

-- | whether there is a winner at current board state
isWinner :: BoardBinRep  -- ^ Current Board
         -> SolutionBin  -- ^ A possible winner state
         -> Maybe Item   -- ^ whether there is a winner.
isWinner b w
  | b .&. w == w                    = Just 'X'
  | b .&. shiftL w 1 == shiftL w 1  = Just 'O'  -- FIXME: duplicated shiftL
  | otherwise                       = Nothing

findWinner :: BoardBinRep -> Maybe Item
findWinner b = msum $ map (isWinner b) winners

{-
  Test Datas
-}

tb :: Board
tb =   "X_O"
    ++ "XOO"
    ++ "X__"

tb1 =  "X_O"
    ++ "OOO"
    ++ "X__"

tb2 =  "X_O"
    ++ "_OO"
    ++ "X__"

tb3 =  "X_O"
    ++ "_OO"
    ++ "X_O"

tb4 =  "O_X"
    ++ "OX_"
    ++ "X__"

testdatas = [tb, tb1, tb2, tb3, tb4]

main :: IO ()
main = mapM_ (print . findWinner . toBoardBinRep) testdatas

