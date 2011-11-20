import Data.List   
 

import Control.Parallel
import Data.Word
 
collatzLen :: Int -> Word32 -> Int
collatzLen c 1 = c
collatzLen c n = collatzLen (c+1) $ if n `mod` 2 == 0 then n `div` 2 else 3*n+1
 
pmax x n = x `max` (collatzLen 1 n, n)
 
solve xs = foldl pmax (1,1) xs
 
main = print s1
    where
        s1 = solve [2..500000]
--        s2 = solve [500001..1000000]
--        soln = s2 `par` (s1 `pseq` max s1 s2)
