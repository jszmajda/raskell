module Raskell.Parser.ASTNodes
( Parenthesis (..)
, BPlus (..)
) where

data Parenthesis = Parenthesis Integer
  deriving (Show, Eq)

data BPlus = BPlus Double Double
  deriving (Show, Eq)
