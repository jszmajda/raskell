module Raskell.Evaluator.Runner
(
  World (..),
  runExpr,
  emptyWorld
) where

import Raskell.ASTNodes
import Raskell.RubyObject
import qualified Data.Map.Lazy as M

data World = World { _world :: (M.Map String RbObject) } deriving (Show, Eq)

emptyWorld :: World
emptyWorld = World M.empty

runExpr :: [Expr] -> World -> ([Expr], World)
runExpr []                         w = ([], w)
-- runExpr ((RubyToken tok exprs):xs) w = (xs, w)
runExpr ((RbInt x):xs)             w = (xs, w)
runExpr (x:xs)                     w = (xs, w) -- eat it, just eat it
