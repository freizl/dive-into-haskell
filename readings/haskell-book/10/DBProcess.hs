-- |
module DBProcess where

import Data.Maybe
import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

unDbDate :: DatabaseItem -> Maybe UTCTime
unDbDate (DbDate tm) = Just tm
unDbDate _ = Nothing

unDbNumber (DbNumber num) = Just num
unDbNumber _ = Nothing

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate
      ( UTCTime
          (fromGregorian 1911 5 1)
          (secondsToDiffTime 34123)
      ),
    DbNumber 9001,
    DbString "Hello, world!",
    DbDate
      ( UTCTime
          (fromGregorian 1921 5 1)
          (secondsToDiffTime 34123)
      )
  ]

isDbDate :: DatabaseItem -> Bool
isDbDate (DbDate _) = True
isDbDate _ = False

isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber _) = True
isDbNumber _ = False


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = map (\(DbDate t) -> t) . filter isDbDate

filterDbDate2 ::[DatabaseItem] -> [UTCTime]
filterDbDate2 = foldr ((\a b -> if isJust a then fromJust a : b else b) . unDbDate) []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = map (\(DbNumber int) -> int) . filter isDbNumber


sumDb :: [DatabaseItem] -> Integer
sumDb = foldr ((\a b -> if isJust a then fromJust a + b else b) . unDbNumber) 0
