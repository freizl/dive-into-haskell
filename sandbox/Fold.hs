module Main where

main :: IO ()
main = do showTodo todoList

-- TODO: is it possible to print array in reverse way using fold

-- @see sequence_
showTodo :: [ IO () ] -> IO ()
showTodo = foldr (>>) (return ())

todoList :: [ IO () ]
todoList = [ putStrLn x | x <- ["AA","BB","CC"] ]
