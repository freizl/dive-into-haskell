-- |

module TooMany where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (i, s) = length s > i

instance TooMany (Int, Int) where
  tooMany (x, y) = x + y > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x1, x2) = or [tooMany x1, tooMany x2, tooMany (x1+x2)]

newtype Goats = Goats Int deriving (Eq, Show, TooMany)
