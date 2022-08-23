{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

-- * Open Type synonym family

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

-- * Closed Type synonym family

type family Widen t where
  Widen Bool = Int
  Widen Int = Integer
  Widen Char = String
  Widen t = String

class Widener a where
  wident :: a -> Widen a

instance Widener Bool where
  wident True = 1
  wident False = 0

instance Widener Int where
  wident = fromIntegral

instance Widener Char where
  wident c = [c]

-- >>> :t wident False
-- wident False :: Int

-- >>> :t wident (1::Int)
-- wident (1::Int) :: Integer

-- >>> :t wident 'a'
-- wident 'a' :: String

-- >>> :kind! Widen Char
-- Widen Char :: *
-- = [Char]

-- >>> :kind! Widen Double
-- Widen Double :: *
-- = [Char]

instance Widener Double where
  wident = show
  -- compile error if not convert to string
  -- because @Widen Double = String@ according to
  -- @Widen t = String@
  -- wident = toInteger


newtype UnescapingChar = UnescapingChar {unescapingChar :: Char}

-- | how to interpret @(a :: k) :: k@
--
type family ToUnescapingTF (a :: k) :: k where
  ToUnescapingTF Char = UnescapingChar
  ToUnescapingTF (t b :: k) = (ToUnescapingTF t) (ToUnescapingTF b)
  ToUnescapingTF a = a

-- >>> :kind! ToUnescapingTF Int

-- >>> :kind! ToUnescapingTF Char

-- >>> :kind! ToUnescapingTF (Maybe Char)

main :: IO ()
main = putStrLn ""
