module Raskell.Parser.ASTNodes
( Expr (..)
) where

data Expr = Int Integer
          | Float Double
          | Var String
          | BPlus Expr Expr
          | Parens Expr
            deriving (Show, Eq)
