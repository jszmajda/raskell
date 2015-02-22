module Main where

{-import Control.Monad (liftM)-}

type Source = String

-- definitely not right, probably should return some sort of environment?
runRuby :: Source -> IO String
runRuby source = return "whatever"

main :: IO ()
main = putStrLn "Hello World"
