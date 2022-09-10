{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}

module HelloDataKinds where

data B = Foo | Bar

-- So @b@ serve as phantom type parameter
-- what other usefulness it has??
--
data C (b :: B) = CX String

t1 :: C 'Foo
t1 = CX "1 foo"

t2 :: C 'Bar
t2 = CX "2 Bar"

f1 :: C 'Foo -> String
f1 (CX s) = s

f2 :: C 'Bar -> String
f2 (CX s) = s

main :: IO ()
main = do
  putStrLn (f1 t1)
  -- putStrLn (f1 t2) -- compile error
  putStrLn (f2 t2)

type family And (x :: B) (y :: B) where
  And 'Foo y = y
  And 'Bar _ = 'Bar
