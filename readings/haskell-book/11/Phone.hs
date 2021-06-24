-- |
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

-- | assuming the defalt phone definition
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
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\x init -> snd x + init) 0

mostPopularLetter :: String -> Char
mostPopularLetter str =
  let allPresses = cellPhonesDead phone str
      grouped = groupBy (\a b -> fst a == fst b) (sort allPresses)
      pressPerDigit = map (\xs -> (fst $ head xs, fingerTaps xs)) grouped
      pl = maximumBy (\a b -> snd a `compare` snd b) pressPerDigit
  in
    fst pl

coolestLtr :: [String] -> Char
coolestLtr = undefined

coolestWord :: [String] -> String
coolestWord = undefined

----------------------------------------

main = defaultMain unitTests

unitTests =
  testGroup
    "reverseTaps"
    [ testCase "a" $ rp 'a' @?= [('2', 1)],
      testCase "b" $ rp 'b' @?= [('2', 2)],
      testCase "d" $ rp 'd' @?= [('3', 1)],
      testCase "e" $ rp 'e' @?= [('3', 2)],
      testCase "A" $ rp 'A' @?= [('*', 1), ('2', 1)],
      testCase "B" $ rp 'B' @?= [('*', 1), ('2', 2)],
      testCase "D" $ rp 'D' @?= [('*', 1), ('3', 1)],
      testCase "E" $ rp 'E' @?= [('*', 1), ('3', 2)],
      testCase "Space" $ rp ' ' @?= [('0', 1)]
    ]
    -- TODO: more test case
  where
    rp = reverseTaps phone
