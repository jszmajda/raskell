module Raskell.Parser.ASTNodes
( Expr (..)
) where

data Expr = Int Integer
          | Float Double
          | BPlus Expr Expr
          | Parens Expr
          | RubyToken String [Expr]
            deriving (Show, Eq)
