module Main where

import Data.List.Split (splitOn)
import Data.List
import Test.QuickCheck

ratesStr :: String
ratesStr = "EURUSD:1.3048;EURGBP:0.8527;EURCAD:1.3367;EURSEK:8.3707;EURCHF:1.2150;"
           ++ "GBPCHF:1.4252;GBPUSD:1.5305;GBPSEK:9.8197;GBPCAD:1.5682;GBPEUR:1.1729;"
           ++ "USDEUR:0.77;USDGBP:0.66;USDCAD:1.03;USDSEK:6.52;USDCHF:0.93;"
           ++ "CADEUR:0.75;CADUSD:0.97;CADGBP:0.64;CADSEK:6.35;CADCHF:0.91;"
           ++ "SEKEUR:0.12;SEKGBP:0.10;SEKUSD:0.15;SEKCAD:0.16;SEKCHF:0.14;"
           ++ "CHFEUR:0.82;CHFGBP:0.70;CHFUSD:1.07;CHFCAD:1.10;CHFSEK:6.99;"

type Currency = String
type FromCurrency = Currency
type ToCurrency = Currency
type Ratio = Float

data CR = CR FromCurrency ToCurrency Ratio
          deriving (Show)

getRatio :: CR -> Ratio
getRatio (CR _ _ r) = r

-- | Maybe instance of `Read`
readCR :: String -> CR
readCR str = let from = take 3 str
                 to = take 3 (drop 3 str)
                 ratio = drop 7 str
             in
             CR from to (read ratio)

allRates :: [CR]
allRates = map readCR $
           filter (not . null) $
           splitOn ";" ratesStr

allCurrency :: [Currency]
allCurrency = nub [ x | (CR x _ _) <- allRates ]

convert :: Float -> Currency -> Currency -> Float
convert number a b = number * (getRatio $ getRate (a, b))

-- | verify interaction 3
--   e.g. EUR -> USD -> GBP V.S. EUR -> GBP

showResult (y, ys, z, zs) =
                 let yStr = show y
                     zStr = show z
                     n = length yStr - length zStr
                     aha = if z - y > 0 then "AHA" else ""
                 in
                 "------" ++ aha ++ "\n"
                 ++ yStr ++ " -> " ++ show ys ++ "\n"
                 ++ zStr ++ replicate n ' ' ++ " -> " ++ show zs

interN :: Int -> IO ()
interN n = writeFile ("inter" ++ show n ++ ".log") $
           concatMap ((++ "\n") . showResult . compareRate) (permXC n)

compareRate :: [Currency] -> (Ratio, [(Currency, Currency)], Ratio, [(Currency, Currency)])
compareRate xs
  | length xs <= 2 = (1, [], 1, [])
  | otherwise = let ys = zip xs (tail xs)
                    yRate = product [ r | y <- ys, CR _ _ r <- [getRate y]]
                            -- yRate becomes a combination rate
                    z = (head xs, last xs)
                    zRate = getRatio (getRate z)
                in
                (yRate, ys, zRate, [z])

-- | NOTES: assum all supported currency having rates.
getRate :: (Currency, Currency) -> CR
getRate (a, b) = head [ cr | cr@(CR f t r) <- allRates, f == a, t == b ]

-- | Permutation

permX :: Eq a => Int -> [a] -> [[a]]
permX 0 _ = [[]]
permX _ [] = [[]]
permX 1 xs = [ [x] | x <- xs ]
permX n xs = [ x:ys | x <- xs, ys <- permX (n-1) xs, x `notElem` ys ]

permXC :: Int -> [[Currency]]
permXC n = permX n allCurrency

perm3 :: [[Currency]]
perm3 = permXC 3

perm4 :: [[Currency]]
perm4 = permXC 4

prop_perm3 :: Bool
prop_perm3 = length perm3 == 120

prop_perm4 :: Bool
prop_perm4 = length perm4 == 360

-- | diff about convert to a currency and convert back
-- EUR -> USD -> EUR
comb2 :: [[Currency]]
comb2 = filter ((== 2) . length) $ subsequences allCurrency

reverseConvert a b = let x = getRate (a, b)
                         y = getRate (b, a)
                     in
                     ([a, b], getRatio x * getRatio y)

-- (["CAD","CHF"],1.001)
-- (["CAD","SEK"],1.0159999)
-- (["EUR","CAD"],1.002525)
-- (["EUR","CHF"],0.99630004)
-- (["EUR","GBP"],1.0001318)
-- (["EUR","SEK"],1.0044839)
-- (["EUR","USD"],1.004696)
-- (["GBP","CAD"],1.0036479)
-- (["GBP","CHF"],0.99763995)
-- (["GBP","SEK"],0.98197)
-- (["GBP","USD"],1.01013)
-- (["SEK","CHF"],0.97859997)
-- (["USD","CAD"],0.9991)
-- (["USD","CHF"],0.9951001)
-- (["USD","SEK"],0.97800004)

main2 :: IO ()
main2 = mapM_ print $
        sort $
        map (\ [a, b] -> reverseConvert a b) comb2

-- | main
main = interN 3
       >> interN 4
       >> interN 5
