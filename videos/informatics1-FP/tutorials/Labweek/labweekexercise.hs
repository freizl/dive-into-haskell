-- Informatics 1 - Functional Programming
-- Lab Week Exercise
--
-- Week 2 - due: Friday, Oct. 1, 5pm
--
-- Insert your name and matriculation number here:
-- Name:
-- Nr. :


import Test.QuickCheck


-- Exercise 3:

double :: Int -> Int
    double x = x + x

square :: Int -> Int
square x = undefined

-- Exercise 4:

isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = undefined


-- Exercise 5:

leg1 :: Int -> Int -> Int
leg1 x y = undefined

leg2 :: Int -> Int -> Int
leg2 x y = undefined

hyp :: Int -> Int -> Int
hyp x y = undefined


-- Exercise 6:

prop_triple :: Int -> Int -> Bool
prop_triple x y = isTriple (leg1 x y) (leg2 x y) (hyp x y)

