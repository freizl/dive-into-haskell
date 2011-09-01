import Char
import List
import Test.QuickCheck
import Text.Printf
import HelloQuickCheck 

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
 
-- reversing twice a finite list, is the same as identity
prop_take5 s = length (take5 s) <= 5
 
tests  = [("take5", quickCheck prop_take5)]
