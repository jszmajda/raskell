module Raskell.RubyObject where

data RbObject = RbObject { _className :: String } deriving (Show, Eq)
