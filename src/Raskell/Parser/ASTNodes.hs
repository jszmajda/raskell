module Raskell.Parser.ASTNodes
( Expr (..)
) where

data Expr = RbInt Integer
          | RbFloat Double
          | RbString String
          | BPlus Expr Expr
          | Parens Expr
          | RubyToken String [Expr]
            deriving (Show, Eq)
