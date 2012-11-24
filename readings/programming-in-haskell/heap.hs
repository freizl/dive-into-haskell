
sumWith1 v [] = v
sumWith1 v (x:xs) = sumWith1 (v+x) xs

sumWith2 v [] = v
sumWith2 v (x:xs) = (sumWith2 $! (v+x)) xs

test1 = sumWith1 0 [1..10000000]
test2 = sumWith2 0 [1..10000000]


main = print test2
       
