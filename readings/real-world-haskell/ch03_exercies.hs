-- file ch03/exercies.hs

myLength (x:xs) = 1 + (myLength xs)
myLength []     = 0
