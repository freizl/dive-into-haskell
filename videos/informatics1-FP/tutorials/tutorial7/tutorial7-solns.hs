-- Informatics 1 - Functional Programming
-- Tutorial 7
--
-- Solutions
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!


import LSystem
import Test.QuickCheck


-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split Sit            = []
split (cmd1 :#: cmd2) =  split cmd1 ++ split cmd2
split cmd            = [cmd]

-- 1b. join
join :: [Command] -> Command
join = foldr (:#:) Sit

-- alternative joins (optimised to avoid extra Sit)
join' :: [Command] -> Command
join' []     = Sit
join' [x]    = x
join' (x:xs) = x :#: join' xs

join'' :: [Command] -> Command
join'' [] = Sit
join'' xs = foldr1 (:#:) xs

prop_join xs = join' xs == join'' xs

-- 1c. equivalent
equivalent :: Command -> Command -> Bool
equivalent cmd1 cmd2 = split cmd1 == split cmd2

-- 1d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join cmd = equivalent (join (split cmd)) cmd

prop_split :: Command -> Bool
prop_split cmd = all f (split cmd)
    where
      f Sit       = False
      f (_ :#: _) = False
      f _         = True


-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy n cmd  |  n <= 0    = Sit
            |  n == 1    = cmd
            |  otherwise = cmd :#: copy (n-1) cmd

-- alternative copy (using join, replicate)
copy' :: Int -> Command -> Command
copy' n cmd = join (replicate n cmd)

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon side = copy 5 (Go side :#: Turn 72.0)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon side nsides =
    copy nsides (Go side :#: Turn angle)
        where angle = 360 / (fromIntegral nsides)



-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral d n s a = sp d n
  where
  sp d n | d <= 0 || n == 0 = Sit
         | otherwise        = Go d :#: Turn a :#: sp (d+s) (n-1)

-- alternative using comprehension
spiral' :: Distance -> Int -> Distance -> Angle -> Command
spiral' d n s a  =
  join [ Go (d + i*s) :#: Turn a | i <- [0..n'-1], d + i*s > 0 ]
  where n' = fromIntegral n

prop_spiral d n s a = d > 0 && n >= 0 ==> equivalent (spiral d n s a) (spiral' d n s a)



-- Exercise 4
-- Remember that Go does not take negative arguments.
optimise :: Command -> Command
optimise = join' . compress . filter (/= Turn 0) . compress . filter (/= Go 0) . split
    where
      compress [] = []
      compress (Turn x : Turn y : xs) = compress (Turn (x+y) : xs)
      compress (Go x : Go y : xs) = compress (Go (x+y) : xs)
      compress (x:xs) = x : compress xs

-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x = f x
    where
      f 0 = GrabPen red :#: Go 10
      f x = g (x-1) :#: p :#: f (x-1) :#: p :#: g (x-1)
      g 0 = GrabPen blue :#: Go 10
      g x = f (x-1) :#: n :#: g (x-1) :#: n :#: f (x-1)
      n = Turn 60
      p = Turn(-60)


-- 6. snowflake
snowflake :: Int -> Command
snowflake x = f x :#: n :#: n :#: f x :#: n :#: n :#: f x :#: n :#: n
    where
      f 0 = Go 10
      f x = f (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1):#: p :#: f (x-1)
      n = Turn 60
      p = Turn(-60)


-- 7. hilbert
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



-- Bonus L-Systems

peanoGosper x = f x
    where
      f 0 = GrabPen red :#: Go 10
      f x = f (x-1) :#: n :#: g (x-1) :#: n :#: n :#: g (x-1) :#: p :#: f (x-1) :#: p :#: p :#: f (x-1) :#: f (x-1) :#: p :#: g (x-1) :#: n
      g 0 = GrabPen blue :#: Go 10
      g x = p :#: f (x-1) :#: n :#: g (x-1) :#: g (x-1) :#: n :#: n :#: g (x-1) :#: n :#: f (x-1) :#: p :#: p :#: f (x-1) :#: p :#: g (x-1)
      n = Turn 60
      p = Turn(-60)


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
