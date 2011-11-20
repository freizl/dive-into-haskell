---
title: summary Some learning notes/tips
author: Haisheng, Wu
tags: haskell, tutorial
---

## Type

The combination of :: and the type after it is called a _type signature_.

~~~
ghci> 'a' :: Char
'a'
ghci> [1,2,3] :: Int

<interactive>:1:0:
    Couldn't match expected type `Int' against inferred type `[a]'
    In the expression: [1, 2, 3] :: Int
    In the definition of `it': it = [1, 2, 3] :: Int
~~~

## Type Class

- notion of class extension

~~~
class (Eq a, Show a) => Num a where
    (+), (-), (*) :: a -> a -> a
~~~

## Miscs

~~~
let x = exp 1    -- let is special for GHCI to assign a variable
[1,2,3]          -- a list
(1,2,"Yes")      -- tuple
a `plus` b = a + b   -- infix function
[1,2] ++ [3,4]       -- 
[1,2] : [3,4]        -- ERROR
:info .  
:info $
::               -- can be read “has type”
~~~

## Monad

~~~
-- debug.out
liftIO $ print sth
~~~
