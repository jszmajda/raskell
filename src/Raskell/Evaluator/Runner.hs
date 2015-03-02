module Raskell.Evaluator.Runner
(
  runExpr,
  emptyWorld
) where

import Raskell.ASTNodes
import Raskell.RubyObject
import qualified Data.Map.Lazy as M

data World = World { _world :: (M.Map String RbObject) } deriving (Show, Eq)

emptyWorld :: World
emptyWorld = World M.empty

runExpr :: [Expr] -> World -> World
runExpr []        w = w
--runExpr (RubyToken "foo" []) w = w
runExpr _         w = w
