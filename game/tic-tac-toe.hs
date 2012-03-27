module Main where

import Data.List (findIndices, transpose)
import Data.Bits
import Control.Monad (msum)

-- | Play 'X' or 'O'.
--   TODO: shall define new data type `TX | TO` ?
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

-- | All possible solutions          
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

-- | Find out whether there a winner on current Board.
--   1. binary compare current board state with all possible winner states
findWinner :: BoardBinRep -> Maybe Item
findWinner b = msum $ map (isWinner b) winners

-- | Matrix representation
type Matrix a = [[a]]
type BoardMatRep = Matrix Char

-- | Sum every rows
sumRows :: BoardMatRep -> [String]
sumRows b = b

-- | Sum every columns
sumCols :: BoardMatRep -> [String]
sumCols = sumRows . transpose

-- | Sum every diagonals
sumDiagonal :: BoardMatRep -> [String]
sumDiagonal b = map (zipWith (\ x y -> y x) b ) [ [head, head . tail, last]
                                                 ,[last, head . tail, head] ]
-- | Transform to Matrix representation
toBoardMatRep :: Board -> BoardMatRep
toBoardMatRep b = let (x,y)   = splitAt 3 b
                      (x1,y1) = splitAt 3 y in
                  [x, x1, y1]
                        
-- | Find out if any winner.
--   1. calculate out 8 scenario in string representation
--   2. filter out those 8 base on it shall be either all 'X' or all 'O'
--   3. present the result using Maybe type. 
findWinnerMatrix :: BoardMatRep -> Maybe Char
findWinnerMatrix b = torep $ filter iswinner $ sumRows b ++ sumCols b ++ sumDiagonal b
                     where iswinner s  = s == "XXX" || s == "OOO"
                           torep []    = Nothing
                           torep (x:_) = Just $ head x

{-
  Test Datas
-}

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
main = do
       print "----------First solution"
       mapM_ (print . findWinner . toBoardBinRep) testdatas
       print "----------Second solution"
       mapM_ (print . findWinnerMatrix . toBoardMatRep) testdatas

