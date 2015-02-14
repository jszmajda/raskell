module Main where

type Source = String

-- definitely not right, probably should return some sort of environment?
runRuby :: Source -> String
runRuby source = "whatever"

main :: IO ()
main = putStrLn "Hello World"
