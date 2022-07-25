-- |

module Rocks where

data Rocks = Rocks String deriving (Eq, Show)

data Yeah = Yeah Bool deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- (x)
-- 1st parameter shall be 'Rocks' instead of String
-- 2nd shall be 'Yeah' instead of Bool
-- phew = Papu "chases" True

-- (/)
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- (/)
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- (x) didn't implement Ord
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'
