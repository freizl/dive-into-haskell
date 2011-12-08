module Test where

type CustomerID = Int
type Address = [String]

data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)

lend amount balance
  | amount <= 0            = Nothing
  | amount > reserve * 0.5 = Nothing
  | otherwise              = Just newBalance
    where reserve    = 100
          newBalance = balance - amount
          
data Roygbiv = Red | Yellow | Green          
             deriving (Eq, Show)
{-- Red == Yellow; Red == Red --}                      

-- [ f x | x <- xs, p x ]
mfilter :: (a -> Bool) -> (a -> a) -> ([a] -> [a])
mfilter p f = map f . filter p

{-- ----------- foldr --}
mapf :: (a -> b) -> [a] -> [b]
mapf f = foldr (\x xs -> (f x):xs ) []

filterf :: (a -> Bool) -> [a] -> [a]
filterf p = foldr (\x xs -> if p x then x:xs else xs) []

reversef :: [a] -> [a]
reversef = foldr (\x xs -> xs ++ [x]) []

lengthf :: [a] -> Int
lengthf = foldr (\x xs -> 1 + xs) 0
