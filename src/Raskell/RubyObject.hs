module Raskell.RubyObject
(
  RbObject (..),
  rubyNumber
) where

data RbObject = RbObject {
                  _className :: String,
                  _value     :: Integer
                }
              | RbFixNum Double
                deriving (Show, Eq)

rubyNumber :: Double -> RbObject
rubyNumber = RbFixNum
