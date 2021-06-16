-- |
module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen =
  "Could frame thy fearful\
  \ symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

notLB x =  x /= '\n'

myLines :: String -> [String]
myLines [] = []
myLines str = let s1 = takeWhile notLB str
                  rest1 = drop 1 (dropWhile notLB str)
              in
                s1: myLines rest1

shouldEqual =
  [ "Tyger Tyger, burning bright",
    "In the forests of the night",
    "What immortal hand or eye",
    "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $
    "Are they equal? " ++ show (myLines sentences == shouldEqual)
