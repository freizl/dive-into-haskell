data Term = Var Var | Lam Var Term | App Term Term 
data Var  = V String

test1 = (App (App (App (App (App (Var (V "a")) (Var (V "b"))) 
                            (Var (V "c")))
                       (Var (V "d")))
                  (Var (V "e"))) 
             (Var (V "f")))

test2 = (App (App (Var (V "g")) (Var (V "h")))
             (App (Var (V "i")) (Var (V "j"))))

test3 = Lam (V "x")
            (App f (Lam y (App f (Var y))))
        where f = Var (V "f")
              y = V "y"

test4 = App (Lam f (App (App (Var f) (Var (V "x")))
                         (Var (V "y"))))
            (Lam x (Lam y (Var x)))
         where f = V "f"
               x = V "x"
               y = V "y"

test5 = App (App (App f g) (Lam x y))
            z
        where f = Var (V "f")
              g = Var (V "g")
              x = V "x"
              y = Var (V "y")
              z = Var (V "z")

main = mapM_ print [test1, test2, test3, test4, test5]

instance Show Var where
    show (V s) = s

instance Show Term where
    show (Var v) = show v
    show (Lam v t) = "λ" ++ show v ++ "." ++ show t
--    show (App t1 t2) = "(" ++ show t1 ++ ") (" ++ show t2 ++ ")"
--    show (App t1 t2) = show1' t1 ++ show2' t2
--                       where show2' t1@(App x y) = "(" ++ show t1 ++ ")"
--                             show1' t1@(Lam x y) = "(" ++ show t1 ++ ")"
--                             show1' (Var v) = show v
