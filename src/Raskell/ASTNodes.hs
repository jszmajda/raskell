module Raskell.ASTNodes
( Expr (..)
) where

data Expr = Int Integer
          | Float Double
          | String String
          | BPlus Expr Expr
          | Parens Expr
          | VarAssign String Expr
          | Token String [Expr]
            deriving (Show, Eq)
