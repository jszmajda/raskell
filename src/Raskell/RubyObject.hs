module Raskell.RubyObject
(
  RbObject (..)
, MethodName
) where


type MethodName = String

data RbObject = Object {
                  _className :: String,
                  _value     :: Integer
                }
              | FixNum Integer
              | Float Double
              | Null
              | Kernel
                deriving (Show, Eq)

