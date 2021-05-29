-- Informatics 1 - Functional Programming 
-- Tutorial 7
--
-- Week 9 - Due: 15/16 Nov.


import LSystem
import Test.QuickCheck

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split = undefined

-- 1b. join
join :: [Command] -> Command
join = undefined

-- 1c  equivalent
equivalent = undefined

-- 1d. testing join and split
prop_split_join = undefined

prop_split = undefined


-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy = undefined

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon = undefined

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon = undefined



-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral = undefined


-- Exercise 4
-- optimise
optimise :: Command -> Command
optimise = undefined



-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead = undefined

-- 6. snowflake
snowflake :: Int -> Command
snowflake = undefined

-- 7. hilbert
hilbert :: Int -> Command
hilbert = undefined

