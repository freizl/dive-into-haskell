module Types where

data Term = Var Var | Lam Var Term | App Term Term 
data Var  = V String

instance Show Var where
  show (V s) = "Var " ++ s
