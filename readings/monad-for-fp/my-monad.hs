{-# LANGUAGE TypeSynonymInstances #-}

module Main where

data Term = Con Int | Div Term Term
          deriving (Show)

term1 = Div (Con 80) (Div (Con 10) (Con 2))

-- | Normal eval without Monad
--   No exception handler or state
--   e.g. `eval (Div (Con 10) (Con 0))` throw errors
--
eval :: Term -> Int
eval (Con i) = i
eval (Div t v) = (eval t) `div` (eval v)

{-

Monad Evaluator

eval (Div t v) = eval t
                 >>= (\a -> eval v
                            >>= \ b -> u (a `div` b))
-}
----------------------------------------
-- Monad
----------------------------------------

class MD m where
  u :: a -> m a
  bind :: m a -> (a -> m b) -> m b

----------------------------------------
-- Exception
----------------------------------------

data ExMonad a = Raise Exception | Return a
               deriving (Show)
type Exception = String

raise :: Exception -> ExMonad a
raise s = Raise s

instance MD ExMonad where
  u a = Return a
  bind m f = case m of
              Raise e -> Raise e
              Return a -> f a

evalEx :: Term -> ExMonad Int
evalEx (Con i) = u i
evalEx (Div t v) = evalEx t
                   `bind` (\a -> evalEx v
                                 `bind` (\b -> case b of
                                                0 -> raise "can div by zeor"
                                                _ -> u (a `div` b)
                                        ))

testEX = evalEx (Div (Con 10) (Con 0))

----------------------------------------
-- State
----------------------------------------

data ST a = ST (State -> (State, a))
type State = Int

tick :: ST ()
tick = ST $ (\x -> (x+1, ()))

tickNext :: a -> ST a
tickNext a = ST $ (\s -> (s+1, a))

instance MD ST where
  u a = ST $ \s -> (s, a)
  --bind :: ST a -> (a -> ST b) -> ST b
  bind (ST f) fn = ST $
                  \s -> let (s1, a1) = f s
                            ST g = fn a1
                            (s2, a2) = g s1
                        in
                         (s2, a2)

evalST :: Term -> ST Int
evalST (Con i) = u i
evalST (Div t v) = evalST t
                   `bind` (\a -> evalST v
                                 `bind`
                                 (\b -> u (a `div` b)
                                        `bind`
                                        tickNext
                                 ))

testST x = let ST f = evalST (Div (Con 80) (Div (Con 10) (Con 2)))
         in
          f x

----------------------------------------
-- Log
----------------------------------------

data Log a = Log (Output, a)
             deriving (Show)

type Output = String

instance MD Log where
    u a = Log ("", a)
    bind m f = let Log (o1, a1) = m
                   Log (o2, a2) = f a1
               in
                Log (o1 ++ o2, a2)

output :: Output -> Log ()
output o = Log (o, ())

lineT :: Term -> Int -> Output
lineT t a = "eval (" ++ show t ++ ") <== " ++ show a ++ "\n"

evalLog :: Term -> Log Int
evalLog (Con i) = output (lineT (Con i) i) `bind` (\a -> u i)
evalLog term@(Div t v) = evalLog t
                     `bind`
                     (\a -> evalLog v
                            `bind`
                            (\b -> output (lineT term (a`div`b))
                                   `bind`
                                   (\c -> u (a `div` b))
                              ))

testLog = evalLog term1
