module Main where

import System.Environment (getArgs)
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
  traceIO $ sourceCode
  traceIO $ show $ parsedAST
  let programContext = emptyProgramContext
  runExprIO ([parsedAST], programContext)
  putStrLn "ok"
