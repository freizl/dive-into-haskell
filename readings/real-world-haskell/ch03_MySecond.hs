-- file: chp03/MySecond.hs

tinySecond :: [a] -> Maybe a

tinySecond (_:x:) = Just x
tinySecond _       = Nothing





