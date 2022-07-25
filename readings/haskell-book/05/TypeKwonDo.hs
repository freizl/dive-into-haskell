-- |

module TypeKwonDo where

{- section 1 -}

f1 :: Int -> String
f1 = undefined
g1 :: String -> Char
g1 = undefined
h1 :: Int -> Char
h1 = g1 . f1

{- section 2 -}
data A
data B
data C

q :: A -> B
q = undefined
w :: B -> C
w = undefined
e :: A -> C
e = w . q

{- section 3 -}
data X
data Y
data Z
xz :: X -> Z
xz = undefined
yz :: Y -> Z
yz = undefined
xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

{- section 4 -}

munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
munge f g x = fst (g (f x))
-- munge f g = fst . g . f
