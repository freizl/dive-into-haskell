-- Informatics 1 - Functional Programming 
-- Lab week tutorial part II
--
--

import PicturesSVG
import Test.QuickCheck



-- Exercise 9:

pic1 :: Picture
pic1 = p `beside` invert p
  where p = knight `above` invert knight

pic2 :: Picture
pic2 = p `above` flipV p
  where p = knight `beside` invert knight

-- Exercise 10:
-- a)

emptyRow :: Picture
emptyRow = repeatH 4 (whiteSquare `beside` blackSquare)

-- b)

otherEmptyRow :: Picture
otherEmptyRow = flipV emptyRow

-- c)

middleBoard :: Picture
middleBoard = repeatV 2 (emptyRow `above` otherEmptyRow)

-- d)

pieces :: Picture
pieces = rook 
         `beside` knight 
         `beside` bishop 
         `beside` queen 
         `beside` king 
         `beside` bishop 
         `beside` knight 
         `beside` rook

whiteRow :: Picture
whiteRow = pieces `over` otherEmptyRow

blackRow :: Picture
blackRow = invert pieces `over` emptyRow

-- e)

pawns :: Picture
pawns = repeatH 8 pawn

populatedBoard :: Picture
populatedBoard = blackPieces `above` middleBoard `above` whitePieces
  where blackPawns  = invert pawns `over` otherEmptyRow
        whitePawns  = pawns `over` emptyRow
        blackPieces = blackRow `above` blackPawns
        whitePieces = whitePawns `above` whiteRow

-- Functions --

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)


-- Exercise 11:

twoAbove :: Picture -> Picture
twoAbove x = above x (invert x)

fourPictures :: Picture -> Picture
fourPictures x = twoAbove (twoBeside bishop)