-- | Chapter Exer

module Exec where

{- Section 1&2 -}

t1 s = s ++ "!"
t2 = take 1 . drop 4
-- NOTE: this has a bit ambiguity
t3 = drop (length "Curry is ")

f1 = do
  putStrLn $ t1 "Curry is awesome"
  putStrLn $ t2 "Curry is awesome!"
  putStrLn $ t3 "Curry is awesome!"


{- Section 3 -}

thirdLetter :: String -> Char
thirdLetter s = s !! 2 -- Not type safe

{- Section 4 -}
letterIndex :: Int -> Char
letterIndex x = input !! x
  where input = "Curry is awesome!"

{- Section 5 -}

-- only works on string "Curry is awesome!"
--
rvrs :: String -> String
rvrs s = let s1 = take 5 s
             s2 = take 2 (drop 6 s)
             s3 = drop 9 s
         in
           s3 ++ " " ++ s2 ++ " " ++ s1

testRvrs :: IO ()
testRvrs = putStrLn (rvrs "Curry is awesome")
