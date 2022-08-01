module Phone where

import Data.Char
import Data.List
import Test.Tasty
import Test.Tasty.HUnit

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and
type Presses = Int

newtype DaPhone = DaPhone [(Digit, [Digit])]
  deriving (Eq, Show)

phone :: DaPhone
phone =
  DaPhone
    [ ('1', []),
      ('2', "ABC"),
      ('3', "DEF"),
      ('4', "GHI"),
      ('5', "JKL"),
      ('6', "MNO"),
      ('7', "PQRS"),
      ('8', "TUV"),
      ('9', "WXYZ"),
      ('*', "%^"), -- Capitalize, ^, *
      ('0', "%+_"), -- Space, +, _, 0
      ('#', ".,")
    ]

convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol OK. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "OK. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"
  ]

-- | assuming the default phone definition
-- 'a' => [('2', 1)]
-- 'A' => [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone []) _ = []
reverseTaps dp@(DaPhone ((a, b) : xs)) c
  | a == c = [(a, length b + 1)]
  | isLower c = go b c
  | isUpper c = ('*', 1) : reverseTaps dp (toLower c)
  | isSpace c = [('0', 1)]
  | otherwise = go b c
  where
    go b c = case elemIndex (toUpper c) b of
      Nothing -> reverseTaps (DaPhone xs) c
      Just i -> (a, i + 1) : reverseTaps (DaPhone xs) c

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead dp = concatMap (reverseTaps dp)

-- | How many times of digits need to be pressed for each message?
--
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\x init -> snd x + init) 0

mostPopularPress :: String -> Presses
mostPopularPress str =
  let popLetter = mostPopularLetter str
      taps = reverseTaps phone popLetter
   in fingerTaps taps

mostPopularWithCount :: (Eq a, Ord a)
  => [a] -> (a, Int)
mostPopularWithCount xs =
  maximumBy (\a b -> snd a `compare` snd b) $
    map (\ys -> (head ys, length ys)) $
      group $
        sort xs

mostPopularLetter :: [Char] -> Char
mostPopularLetter = fst . mostPopularWithCount . filter isLetter

-- | What is the most popular letter overall?
--
coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

-- | What is the overall most popular word?
--
coolestWord :: [String] -> String
coolestWord = fst . mostPopularWithCount . concatMap words

----------------------------------------

main :: IO ()
main = do
  print $ coolestLtr convo
  print $ coolestWord convo

mainTest :: IO ()
mainTest = do
  defaultMain unitTests

unitTests :: TestTree
unitTests =
  testGroup
    "reverseTaps"
    [ testCase "1" $ rp '1' @?= [('1', 1)],
      testCase "2" $ rp '2' @?= [('2', 4)],
      testCase "3" $ rp '3' @?= [('3', 4)],
      testCase "4" $ rp '4' @?= [('4', 4)],
      testCase "5" $ rp '5' @?= [('5', 4)],
      testCase "6" $ rp '6' @?= [('6', 4)],
      testCase "7" $ rp '7' @?= [('7', 5)],
      testCase "8" $ rp '8' @?= [('8', 4)],
      testCase "9" $ rp '9' @?= [('9', 5)],
      testCase "0" $ rp '0' @?= [('0', 4)],
      testCase "a" $ rp 'a' @?= [('2', 1)],
      testCase "b" $ rp 'b' @?= [('2', 2)],
      testCase "d" $ rp 'd' @?= [('3', 1)],
      testCase "e" $ rp 'e' @?= [('3', 2)],
      testCase "A" $ rp 'A' @?= [('*', 1), ('2', 1)],
      testCase "B" $ rp 'B' @?= [('*', 1), ('2', 2)],
      testCase "C" $ rp 'C' @?= [('*', 1), ('2', 3)],
      testCase "D" $ rp 'D' @?= [('*', 1), ('3', 1)],
      testCase "E" $ rp 'E' @?= [('*', 1), ('3', 2)],
      testCase "F" $ rp 'F' @?= [('*', 1), ('3', 3)],
      testCase "G" $ rp 'G' @?= [('*', 1), ('4', 1)],
      testCase "H" $ rp 'H' @?= [('*', 1), ('4', 2)],
      testCase "I" $ rp 'I' @?= [('*', 1), ('4', 3)],
      testCase "J" $ rp 'J' @?= [('*', 1), ('5', 1)],
      testCase "K" $ rp 'K' @?= [('*', 1), ('5', 2)],
      testCase "L" $ rp 'L' @?= [('*', 1), ('5', 3)],
      testCase "M" $ rp 'M' @?= [('*', 1), ('6', 1)],
      testCase "N" $ rp 'N' @?= [('*', 1), ('6', 2)],
      testCase "O" $ rp 'O' @?= [('*', 1), ('6', 3)],
      testCase "P" $ rp 'P' @?= [('*', 1), ('7', 1)],
      testCase "Q" $ rp 'Q' @?= [('*', 1), ('7', 2)],
      testCase "R" $ rp 'R' @?= [('*', 1), ('7', 3)],
      testCase "S" $ rp 'S' @?= [('*', 1), ('7', 4)],
      testCase "T" $ rp 'T' @?= [('*', 1), ('8', 1)],
      testCase "U" $ rp 'U' @?= [('*', 1), ('8', 2)],
      testCase "V" $ rp 'V' @?= [('*', 1), ('8', 3)],
      testCase "W" $ rp 'W' @?= [('*', 1), ('9', 1)],
      testCase "X" $ rp 'X' @?= [('*', 1), ('9', 2)],
      testCase "Y" $ rp 'Y' @?= [('*', 1), ('9', 3)],
      testCase "Z" $ rp 'Z' @?= [('*', 1), ('9', 4)],
      testCase "Space" $ rp ' ' @?= [('0', 1)]
    ]
  where
    rp = reverseTaps phone
