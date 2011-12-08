module Parsing where

import Monad
import Char

infixr 5 +++

newtype Parser a = P (String -> [(a, String)])

instance Monad Parser where
  return v = P (\inp -> [(v, inp)])
  p >>= f  = P (\inp -> case parse p inp of
                           [] -> []
                           [(v, out)] -> parse (f v) out)
instance MonadPlus Parser where             
  mzero        = P (\inp -> [])
  p `mplus` q  = P (\inp -> case parse p inp of
                       []   ->  parse q inp
                       [(a,out)]  -> [(a,out)])
                       
failure :: Parser a
failure = mzero

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
             [] -> []
             (x:xs) -> [(x, xs)])
             
item2 :: Parser (Char, Char)
item2 = do
    x <- item
    item
    y <- item
    return (x, y)
        
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q

-- Derived primitives
sat   :: (Char -> Bool) -> Parser Char
sat p =  do x <- item
            if p x then return x else failure             
             
digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

alphanum :: Parser Char
alphanum = sat isAlphaNum

hasChar   :: Char -> Parser Char
hasChar x =  sat (== x)

hasString   :: String -> Parser String
hasString []     = return []
hasString (x:xs) = do hasChar x
                      hasString xs
                      return (x:xs)
                      
--- Confusing stuffs??
many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)
---             
             
ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)
            
nat :: Parser Int             
nat = do xs <- many1 digit
         return (read xs)
         
int :: Parser Int         
int = do hasChar '-'
         n <- nat
         return (-n)
       +++ nat
      
space :: Parser ()
space = do many (sat isSpace)
           return ()

token   :: Parser a -> Parser a
token p = do space 
             v <- p
             space
             return v
             
identifier :: Parser String             
identifier = token ident
                
natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (hasString xs)

---------------------------------- Exercise
comment :: Parser ()
comment = do symbol "--"
             vs <- many ident
             symbol "\n"           -- ?? How to consume \n
             return ()
             
