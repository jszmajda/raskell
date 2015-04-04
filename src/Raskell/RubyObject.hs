module Raskell.RubyObject
(
  RbObject (..),
  rubyNumber
) where

data RbObject = Object {
                  _className :: String,
                  _value     :: Integer
                }
              | FixNum Double
              | Float Double
              | Null
                deriving (Show, Eq)

rubyNumber :: Double -> RbObject
rubyNumber = FixNum
