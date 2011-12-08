-- file: chp02/lastButOne.hs

lastButOne :: [a] -> a
lastButOne xs = if null xs || (length xs) < 3
                then head xs
                else lastButOne (tail xs)
