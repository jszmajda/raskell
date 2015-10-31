module Raskell.Evaluator.Runner
(
  ProgramContext (..),
  runExpr,
  runExprIO,
  evalExpr,
  emptyProgramContext
) where

import Raskell.Evaluator.ProgramContext
import qualified Raskell.ASTNodes as AST
import qualified Raskell.RubyObject as Rb
import Raskell.PrimImpl
import qualified Data.Map as M
import Debug.Trace (traceShow)
import Control.Monad (liftM)
import System.IO.Unsafe (unsafePerformIO)

type ProgramState = ([AST.Expr], ProgramContext)

---------------------------------------- IO
-- Maybe in the future we want IOExpr's and non-IOExpr's?
runExprIO :: ProgramState -> IO ProgramState
-- runExprIO (((AST.Token "puts" exprList):xs), w) = runPuts exprList (xs, w)
runExprIO (((AST.Token token exprList):xs),  ctxt) = runTokenExpr token exprList (xs, ctxt)
runExprIO programState = return $ runExpr programState

runTokenExpr :: String -> [AST.Expr] -> ProgramState -> IO ProgramState
runTokenExpr token args state@(xs, ctxt) = something >> return state
  where
    something     = invokeMethodIO ctxt (currentSelf ctxt) token evaluatedArgs
    evaluatedArgs = map (flip evalExpr $ ctxt) args

-- I guess I don't need this? What about if an expression causes IO?
-- evalExprIO :: AST.Expr -> ProgramContext -> IO Rb.RbObject
-- evalExprIO expr ctxt = evalExpr expr ctxt

---------------------------------------- Non IO
runExpr :: ProgramState -> ProgramState
runExpr ([]                              ,ctxt) = ([], ctxt)
runExpr (((AST.VarAssign var val):xs)    ,ctxt) = (xs, varAssign var val ctxt)
runExpr (((AST.Int x):xs)                ,ctxt) = (xs, ctxt)
runExpr ((x:xs)                          ,ctxt) = (xs, ctxt) -- eat it, just eat it


currentSelf :: ProgramContext -> Rb.RbObject
currentSelf _ = Rb.Kernel

evalExpr :: AST.Expr -> ProgramContext -> Rb.RbObject
evalExpr (AST.Int   x) ctxt = Rb.FixNum $ fromIntegral x
evalExpr (AST.Float x) ctxt = Rb.Float x
evalExpr (AST.BPlus lhs rhs) ctxt = evalBPlus lhs rhs ctxt
evalExpr x             ctxt = Rb.Null

evalBPlus :: AST.Expr -> AST.Expr -> ProgramContext -> Rb.RbObject
evalBPlus lhs rhs ctxt = invokeMethod ctxt left "+" [right]
  where
    left  = evalExpr lhs ctxt
    right = evalExpr rhs ctxt

varAssign :: String -> AST.Expr -> ProgramContext -> ProgramContext
varAssign var val ctxt = ProgramContext $ M.insert var resolvedVal (_contextMap ctxt)
  where resolvedVal = evalExpr val ctxt
