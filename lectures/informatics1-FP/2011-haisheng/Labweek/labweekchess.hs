-- Informatics 1 - Functional Programming 
-- Lab week tutorial part II
--
-- NOTES
-- my solution is TOO annoying compare to the solution.
--

import PicturesSVG
import Test.QuickCheck

main :: IO ()
main = render populatedBoard

-- Exercise 9:

pic1 :: Picture
pic1 = knight `beside` blackKnight
       `above`
       (blackKnight `beside` knight)

blackKnight :: Picture
blackKnight = invert knight

pic2 :: Picture
pic2 = knight `beside` blackKnight
       `above`
       ((flipV blackKnight) `beside` (flipV knight))


-- Exercise 10:
-- a)

emptyRow :: Picture
emptyRow = composeRow [] whiteBlack

whiteBlack = [w,b, w,b, w,b, w,b]
blackWhite = [b, w,b, w,b, w, b, w]

w = whiteSquare
b = blackSquare

-- b)

otherEmptyRow :: Picture
otherEmptyRow = composeRow [] blackWhite

-- c)

middleBoard :: Picture
middleBoard = repeatV 2 (emptyRow `above` otherEmptyRow)

-- d)

whiteRow :: Picture
whiteRow = composeRow row0 blackWhite
           
blackRow :: Picture
blackRow = composeRow (map invert row0) whiteBlack

pawnWhiteRow :: Picture
pawnWhiteRow = composeRow row1 whiteBlack

pawnBlackRow :: Picture
pawnBlackRow = composeRow (map invert row1) blackWhite

composeRow :: [Picture] -> [Picture] -> Picture
composeRow [] bg = foldl1 beside bg
composeRow fg bg = foldl1 beside (zipWith over fg bg)
               
row0 = [ rook, knight, bishop, queen, king, bishop, knight, rook ]
row1 = replicate 8 pawn
  

-- e)

populatedBoard :: Picture
populatedBoard = foldl1 above $
                 [ blackRow
                 , pawnBlackRow
                 , middleBoard
                 , pawnWhiteRow
                 , whiteRow ]

-- Functions --

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)


-- Exercise 11:

twoAbove :: Picture -> Picture
twoAbove x = x `above` invert x

fourPictures :: Picture -> Picture
fourPictures = twoAbove . twoBeside
