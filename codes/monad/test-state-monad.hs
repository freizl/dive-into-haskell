{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad.Trans.State.Lazy

-- | The state
type S = String

sayHello :: S -> S
sayHello = (++ "Hello, ")

sayWorld :: S -> S
sayWorld = (++ "World")

-- | Play with just State
taskOne :: State S a -> S
taskOne = flip execState ""
          . withState sayHello
          . withState sayWorld

initState :: Monad m => StateT S m ()
initState = StateT (\s ->  return ((), s))

-- | Play with State Monad
taskTwo :: Monad m => StateT S m S
taskTwo = modify sayHello
          >> modify sayWorld
          >> get

main = do
       let s1 = taskOne initState
       print ("Task one: " ++ s1)
       (s2, _) <- runStateT taskTwo ""
       print ("Task two: " ++ s2)
