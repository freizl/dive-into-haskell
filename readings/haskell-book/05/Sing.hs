-- |

module Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

--     or x < y
sing = if x > y then fstString x else sndString y
 where
  x = "Singin"
  y = "Somewhere"
