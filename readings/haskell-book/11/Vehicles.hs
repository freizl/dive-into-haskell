-- |

module Vehicles where

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata
  deriving (Show, Eq)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
  deriving (Show, Eq)

type Size = Int

data Vehicle =
  Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)


myCar :: Vehicle
myCar = Car Mini (Price 14000)

urCar :: Vehicle
urCar = Car Mazda (Price 20000)

clownCar :: Vehicle
clownCar = Car Tata (Price 7000)

doge :: Vehicle
doge = Plane PapuAir 100


isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar


getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _ = error "getManu: no manufacturer for non-car vehicle"
