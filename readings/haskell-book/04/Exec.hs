-- | 

module Exec where

awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

-- length :: [a] -> Int


{- Section 8 -}

isPalindrome :: Eq a => [a] -> Bool
isPalindrome s = s == reverse s

{- Section 9 -}

myAbs :: Integer -> Integer
myAbs x = if x < 0 then x * (-1) else x

{- Section 10 -}
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

{- Section 11 -}
x = (+)
f11 xs = w `x` 1
  where w = length xs

f12 = \x -> x

f13 (a, b) = a
