-- Informatics 1 - Functional Programming
-- Tutorial 7
--
-- Week 9 - Due: 15/16 Nov.


import LSystem
import Test.QuickCheck

-- Exercise 1

testCommand :: Command
testCommand = Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 30

-- 1a. split
split :: Command -> [Command]
split Sit = []
split (a :#: b) = split a ++ split b
split a = [a]

-- 1b. join
join :: [Command] -> Command
join [] = Sit
join [c] = c
join (c:cs) = c :#: join cs

-- 1c  equivalent
equivalent :: Command -> Command -> Bool
equivalent c1 c2 = split c1 == split c2

-- 1d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join c = (join $ split c) `equivalent` c

prop_split :: Command -> Bool
prop_split c = checkAll (split c)
               where checkAll = all checked
                     checked Sit = False
                     checked (a :#: b) = False
                     checked _ = True

-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
-- copy n = foldr (:#:) Sit . replicate n
copy n c
  | n <= 1 = c
  | otherwise = c :#: copy (n-1) c

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon l = polygon l 5

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon l n = let angle = 360 / fromIntegral n
                  c = Go l :#: Turn angle
              in
              copy n c



-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral d n step angle
  | n <= 1    = Go d :#: Turn angle
  | step <= 0 = Go d :#: Turn angle
  | otherwise = Go d :#: Turn angle :#: spiral (d+step) (n-1) step angle

testSpiral1 :: IO ()
testSpiral1 = display $ spiral 0.1 1000 0.1 4

-- Exercise 4
-- optimise
optimise :: Command -> Command
optimise c = join $ opt $ split c
             where opt [] = []
                   opt [a] = [a]
                   opt (a:b:cs) = opt (rule2 a b : opt cs)

rule2 :: Command -> Command -> Command
rule2 (Go 0) c = c
rule2 c (Go 0) = c
rule2 (Turn 0) c = c
rule2 c (Turn 0) = c
rule2 (Go a) (Go b) = Go (a+b)
rule2 (Turn a) (Turn b) = Turn (a+b)
rule2 a b = a :#: b


testOpimise1 :: Command
testOpimise1 = optimise (Go 10 :#: Sit :#: Go 20 :#:
                         Turn 35 :#: Go 0 :#: Turn 15 :#: Turn (-50))

testBranch :: Command
testBranch = let inDirection angle = Branch (Turn angle :#: Go 100) in
             join (map inDirection [20,40..360])

-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x = f x
  where f 0 = GrabPen red :#: Go 10
        f x = g (x-1) :#: p :#: f (x-1) :#: p :#: g (x-1)
        g 0 = GrabPen blue :#: Go 10
        g x = f (x-1) :#: n :#: g (x-1) :#: n :#: f (x-1)
        p = Turn (-60)
        n = Turn 60

-- 6. snowflake
{-
  angle: 60
  start: f--f--f--
  rewrite: f → f+f--f+f
-}
snowflake :: Int -> Command
snowflake x = f x :#: n :#: n :#: f x :#: n :#: n :#: f x :#: n :#: n
  where f 0 = Go 10
        f x = f (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1) :#: p :#: f (x-1)
        p = Turn (-60)
        n = Turn 60

-- 7. hilbert
{-
  angle: 90
  start: l
  rewrite:
    l → +rf-lfl-fr+
    r → -lf+rfr+fl-
-}
hilbert :: Int -> Command
hilbert x = l x
    where
      l 0 = Sit
      l x = p :#: r (x-1) :#: f :#: n :#: l (x-1) :#: f :#: l (x-1) :#: n :#: f :#: r (x-1) :#: p
      r 0 = Sit
      r x = n :#: l (x-1) :#: f :#: p :#: r (x-1) :#: f :#: r (x-1) :#: p :#: f :#: l (x-1) :#: n
      f = GrabPen black :#: Go 10
      n = Turn 90
      p = Turn(-90)


-- TODO: diff with solution ??
peanoGosper :: Int -> Command
peanoGosper x = f x
  where f 0 = GrabPen red :#: Go 10
        f x = f (x-1) :#: p :#: g (x-1) :#: p :#: p :#: g (x-1) :#: n
              :#: f (x-1) :#: n :#: n :#: f (x-1) :#: f (x-1) :#: n
              :#: g (x-1) :#: p
        g 0 = GrabPen blue :#: Go 10
        g x = n :#: f (x-1) :#: p :#: g (x-1) :#: g (x-1) :#: p :#: p
              :#: g (x-1) :#: f (x-1) :#: n :#: n
              :#: f (x-1) :#: n :#: g (x-1)
        n = Turn 60
        p = Turn (-60)
peanoGosper2 x = f x
    where
      f 0 = GrabPen red :#: Go 10
      f x = f (x-1) :#: n :#: g (x-1) :#: n :#: n :#: g (x-1) :#: p :#: f (x-1) :#: p :#: p :#: f (x-1) :#: f (x-1) :#: p :#: g (x-1) :#: n
      g 0 = GrabPen blue :#: Go 10
      g x = p :#: f (x-1) :#: n :#: g (x-1) :#: g (x-1) :#: n :#: n :#: g (x-1) :#: n :#: f (x-1) :#: p :#: p :#: f (x-1) :#: p :#: g (x-1)
      n = Turn 60
      p = Turn(-60)

-- copied from solution
cross :: Int -> Command
cross x = f x :#: n :#: f x :#: n :#: f x :#: n :#: f x
    where
      f 0 =  Go 10
      f x = f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1)
      n = Turn 90
      p = Turn(-90)




branch x = g x
   where
     g 0 = GrabPen red :#: Go 10
     g x = f (x-1) :#: p :#: Branch (Branch (g (x-1)) :#: n :#: g (x-1)) :#: f (x-1) :#: Branch (n :#: f (x-1) :#: g (x-1)) :#: p :#: g (x-1)
     f 0 = GrabPen blue :#: Go 10
     f x = f (x-1) :#: f (x-1)
     n = Turn 22.5
     p = Turn(-22.5)


thirtytwo x = f x :#: n :#: f x :#: n :#: f x :#: n :#: f x
    where
      f 0 = Go 10.0
      f x =  p :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: n
      n = Turn 90
      p = Turn (-90)

main :: IO ()
main = print "undefined"
