---
title: Code
---

# IO Monad

Demostrate that `IO monad` is just a `type`.

TODO: is it possible to print array in reverse way using fold

> main :: IO ()
> main = do showTodo todoList
> 
> showTodo :: [ IO () ] -> IO ()
> showTodo = foldr (>>) (return ())
> 
> todoList :: [ IO () ]
> todoList = [ putStrLn x | x <- ["AA","BB","CC"] ]
>> -- product result
>> -- AA
>> -- BB
>> -- CC

# Arrows

+ [arrows-syntax](http://www.haskell.org/arrows/syntax.html)
+ [monad-2-arrows](http://www.cse.chalmers.se/~rjmh/)

