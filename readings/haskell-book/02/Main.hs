-- |

module Main where

sayHello :: String -> IO ()
sayHello x = putStrLn ("hello, " ++ x ++ "!")

triple x = x * 3

area x = pi * x

main :: IO ()
main = sayHello "hw"
