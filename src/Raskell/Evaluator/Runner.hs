module Raskell.Evaluator.Runner
(
  World (..),
  runExpr,
  evalExpr,
  emptyWorld
) where

import qualified Raskell.ASTNodes as AST
import qualified Raskell.RubyObject as Rb
import qualified Data.Map.Lazy as M

data World = World { _world :: (M.Map String Rb.RbObject) } deriving (Show, Eq)

emptyWorld :: World
emptyWorld = World M.empty

runExpr :: [AST.Expr] -> World -> ([AST.Expr], World)
runExpr []                         w = ([], w)
-- runExpr ((RubyToken tok exprs):xs) w = (xs, w)
runExpr ((AST.VarAssign var val):xs) w = (xs, varAssign var val w)
runExpr ((AST.Int x):xs)             w = (xs, w)
runExpr (x:xs)                       w = (xs, w) -- eat it, just eat it

evalExpr :: AST.Expr -> World -> Rb.RbObject
evalExpr (AST.Int   x) w = Rb.FixNum $ fromIntegral x
evalExpr (AST.Float x) w = Rb.Float x
evalExpr x             w = Rb.Null

-- Pending evalExpr
varAssign :: String -> AST.Expr -> World -> World
varAssign var val w = World $ M.insert var resolvedVal (_world w)
  where resolvedVal = evalExpr val w
