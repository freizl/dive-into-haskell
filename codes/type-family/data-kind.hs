{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Main where

type Address = String
data ConnectionStatus = Open | Closed
data Connection (s :: ConnectionStatus) = MkConnection
  { status :: ConnectionStatus
  , address :: Address
  }

{-
-}
newConnection     :: Address           -> Connection Closed
newConnection addr = MkConnection { status = Closed, address = addr }

openConnection    :: Connection Closed -> Connection Open
openConnection = undefined

closeConnection   :: Connection Open   -> Connection Closed
closeConnection = undefined

connectionAddress :: Connection s      -> Address
connectionAddress = undefined

{-
c1 could be either Open or Closed. Guess it is because
`s` is an phantom type.
Is there way to use the `s` in data constructor?
-}
-- c1 :: Connection Open
c1 :: Connection Closed
c1 = MkConnection { status = Open, address = "1.1.1.1"}

main :: IO ()
main = putStrLn "Hello"
