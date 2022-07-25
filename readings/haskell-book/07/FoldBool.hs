-- |

module FoldBool where


foldBool :: a -> a -> Bool -> a
foldBool x _ False = x
foldBool _ y True = y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y cond = case cond of
  True -> y
  False -> x

foldBool3 :: a -> a -> Bool -> a
foldBool3 x y cond
  | cond == True  = y
  | cond == False = x
