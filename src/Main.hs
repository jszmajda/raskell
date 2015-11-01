module Main where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Raskell.Parser.RubyParser (parseRubySource)
import Raskell.Evaluator.Runner (runExprIO, emptyProgramContext)
import Debug.Trace
{-import Control.Monad (liftM)-}

type Source = String

main :: IO ()
main = do
  args <- getArgs
  -- file = head args
  sourceCode <- readFile $ head args
  let parsedAST = parseRubySource sourceCode
  let programContext = emptyProgramContext
  -- We can use traceIO to look at what's going on under the hood
  runExprIO ([parsedAST], programContext)
  exitWith ExitSuccess
