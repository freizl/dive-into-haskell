-- |
module HuttonRazor where

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add e1 e2) = printExpr e1 ++ " + " ++ printExpr e2


main :: IO ()
main = do
  let e1 = Add (Lit 9001) (Lit 1)
  let e2 = Add e1 (Lit 20001)
  let e3 = Add (Lit 1) e2
  mapM_ (\e -> print (printExpr e) >> print (eval e)) [e1,e2,e3]
