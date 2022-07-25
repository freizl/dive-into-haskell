-- |

module TypeKwonDo where

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = b == (f a)

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f x a =
  f a
  +    -- can choose any op from Num type class
  fromInteger x -- has to convert Integer to the Num instance, same to `b`
