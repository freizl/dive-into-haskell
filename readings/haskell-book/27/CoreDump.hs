module CoreDump where

discriminatory :: Bool -> Int
discriminatory b =
  case b of
    True -> 1
    False -> 0

disc2 :: Bool -> Int
disc2 b =
  let x = undefined
   in case x `seq` b of
        True -> 1
        False -> 0
