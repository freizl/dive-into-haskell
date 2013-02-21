module Main where

import Data.List
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T

type Length = Integer
type Fee = Integer
type Income = Integer

data MonthType = MonthType Length Fee Income

maxMonthLength :: Length
maxMonthLength = 36

showBaseOnMaxLength :: MonthType -> Text
showBaseOnMaxLength x@(MonthType l f i) = T.pack $
                                          "Month " ++ show l
                                          ++ ", 月租费 " ++ show f
                                          ++ ", 反充金额 " ++ show i
                                          -- ++ ". " ++ show maxMonthLength ++ "月内"
                                          ++ ": 总计话费 " ++ (show $ maxMonthLength * f)
                                          ++ ", 自己支付1 " ++ show (sol1 x)
                                          ++ ", 自己支付2 " ++ show (sol2 x)


sol1, sol2 :: MonthType -> Integer
-- | 套餐过后使用96套餐
sol2 (MonthType l f i) = l * f + (maxMonthLength - l) * 96 - i
-- | 三年内都使用同一个套餐, 即套餐过期后需要自己全额支付套餐费.
sol1 (MonthType _ f i) = maxMonthLength * f - i

iphone5 :: [MonthType]
iphone5 = [ MonthType 12 96 1400
          , MonthType 12 126 1600
          , MonthType 12 156 1700
          , MonthType 12 186 1900
          , MonthType 12 226 2100
          , MonthType 12 286 2400
          , MonthType 12 386 3000
          , MonthType 12 586 4100
          , MonthType 12 886 5899
          , MonthType 24 96 1900
          , MonthType 24 126 2300
          , MonthType 24 156 2600
          , MonthType 24 186 2900
          , MonthType 24 226 3300
          , MonthType 24 286 4000
          , MonthType 24 386 5899
          , MonthType 24 586 5899
          , MonthType 24 886 5899
          , MonthType 36 96 2400
          , MonthType 36 126 2900
          , MonthType 36 156 3400
          , MonthType 36 186 3900
          , MonthType 36 226 4600
          , MonthType 36 286 5899
          , MonthType 36 386 5899
          , MonthType 36 586 5899
          , MonthType 36 886 5899
          ]

iphone4 :: [MonthType]
iphone4 = [ MonthType 12 96 1200
          , MonthType 12 126 1400
          , MonthType 12 156 1500
          , MonthType 12 186 1700
          , MonthType 12 226 1900
          , MonthType 12 286 2200
          , MonthType 12 386 2800
          , MonthType 12 586 3800
          , MonthType 12 886 4899
          , MonthType 24 96 1700
          , MonthType 24 126 2000
          , MonthType 24 156 2400
          , MonthType 24 186 2700
          , MonthType 24 226 3100
          , MonthType 24 286 3800
          , MonthType 24 386 4899
          , MonthType 24 586 4899
          , MonthType 24 886 4899
          , MonthType 36 96 2200
          , MonthType 36 126 2700
          , MonthType 36 156 3200
          , MonthType 36 186 3700
          , MonthType 36 226 4899
          , MonthType 36 286 4899
          , MonthType 36 386 4899
          , MonthType 36 586 4899
          , MonthType 36 886 4899
          ]


minPayP :: MonthType -> MonthType -> Ordering
minPayP a b = sol1 a `compare` sol1 b

earnedP :: MonthType -> Bool
earnedP (MonthType l f i) = i - l * f > 0

main :: IO ()
main = do
  print ">> for iphone 5"
  calx iphone5
  print ">> for iphone 4"
  calx iphone4

calx :: [MonthType] -> IO ()
calx xs = let xs1 = sortBy minPayP xs
          in
          mapM_ (T.putStrLn . showBaseOnMaxLength) xs1
