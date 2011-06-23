
myflatten [] = []
myflatten (x:xs) = x:(myflatten xs)
