module Types where

data Term = Var Var | Lam Var Term | App Term Term 
data Var  = V String
            deriving (Eq,Show)
