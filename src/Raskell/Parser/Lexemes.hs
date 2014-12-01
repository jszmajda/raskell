module Raskell.Parser.Lexemes
( Parenthesis (..)
, UPlus (..)
) where

data Parenthesis = Parenthesis Integer
  deriving (Show, Eq)

data UPlus = UPlus Double Double
  deriving (Show, Eq)
