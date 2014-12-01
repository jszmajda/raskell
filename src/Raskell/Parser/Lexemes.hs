module Raskell.Parser.Lexemes
( Parenthesis (..)
) where

data Parenthesis = Parenthesis Integer
  deriving (Show, Eq)
