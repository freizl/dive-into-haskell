-- | Converting numbers to words

module Main where


units, teens, tens :: [String]
units = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
teens = [ "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
          "seventeen", "eighteen", "nineteen" ]
tens = [ "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety" ]

convert2 :: Int -> String
convert2 n = combine2 (digits2 n)

digits2 :: Int -> (Int, Int)
digits2 n = (n `div` 10, n `mod` 10)

combine2 :: (Int, Int) -> String
combine2 (0, j) = units !! (j-1)  -- 1-9
combine2 (1, j) = teens !! j      -- 10-19
combine2 (i, 0) = teens !! (i-2)  -- 20
combine2 (i, j) = tens !! (i-2) ++ "-" ++ units !! (j-1) -- 21-99

convert3 n = combine3 (digits3 n)
digits3 n = (n `div` 100, n `mod` 100)
combine3 (0, j) = convert2 j
combine3 (i, 0) = units !! (i-1) ++ " hundred"
combine3 (i, j) = units !! (i-1) ++ " hundred and " ++ convert2 j

convert6 n = combine6 (digits6 n)
digits6 n = (n `div` 1000, n `mod` 1000)
combine6 (0, j) = convert3 j
combine6 (i, 0) = convert3 i ++ " thousand"
combine6 (i, j) = convert3 i ++ " thousand" ++ links j ++ convert3 j
                  where links m
                          | m < 100 = " and "
                          | otherwise = " "

simpleTest :: IO ()
simpleTest = mapM_ (print . convert6) [308000, 369027, 369401]

------------------------------
-- Exercise
------------------------------

--   ?? what is a full-stop character
