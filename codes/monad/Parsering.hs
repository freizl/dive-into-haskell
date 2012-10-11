module Parsering where

type Parser a = String -> [(a, String)]

item :: Parser Char
item = \inp -> case inp of
  [] -> []
  (x:xs) -> [(x, xs)]

failure :: Parser a
failure = \inp -> []

-- TODO: how to avoid confilict with Preclude.return
return :: a -> Parser a
return x = \inp -> [(x, inp)]

(+++) :: Parser a -> Parser a -> Parser a                   
(+++) p q = \inp -> case p inp of
  [] -> parse q inp
  [(v,k)] -> [(v,k)]

parse :: Parser a -> String -> [(a, String)]
parse p v = p v

p1 :: Parser (Char,Char)
p1 = do x <- item
        item
        y <- item
        Parsering.return (x,y) -- compilatio failed
