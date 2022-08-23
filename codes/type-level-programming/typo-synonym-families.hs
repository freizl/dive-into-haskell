{-# LANGUAGE TypeFamilies #-}
-- |

module Main where

type family Simplify t

type instance Simplify Integer = Integer
type instance Simplify Double = Integer
type instance Simplify Float = Integer
type instance Simplify Bool = String

-- >>> :kind Simplify

-- >>> :kind (Simplify Double)
-- (Simplify Double) :: *

-- >>> :kind! (Simplify Double)
-- (Simplify Double) :: *
-- = Integer


main = putStrLn ""
