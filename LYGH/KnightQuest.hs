module KnightQuests where

{-- |
  http://learnyouahaskell.com/a-fistful-of-monads
--}

import Control.Monad(foldM)

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = filter onBoard  
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
    ]  
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8] 

in3 :: KnightPos -> [KnightPos]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

inx :: Int -> KnightPos -> [KnightPos]
inx start n = foldM (\a _ -> moveKnight a) start [1..n]


canReachViaIn3 :: KnightPos -> KnightPos -> Bool
canReachViaIn3 start end = end `elem` (inx start 3)

-- TODO print out moving path when found

{-- Tree Implementation --}

-- | One Node has 8 possible direction to move, use list as general
data PathTree = Node KnightPos [KnightPos]

--route :: Int -> KnightPos -> [KnightPos]
--route n start = foldr (\a _ -> moveKnight a) start [1..n]

-- moveKnight2 :: KnightPos -> PathTree
-- moveKnight2 start = Node start (moveKnight start)
-- 
-- moveKnightx :: PathTree -> Int -> PathTree
-- moveKnightx p 0 = p
-- moveKnightx (Node s []) n = moveKnightx (Node s (moveKnight s)) n-1
