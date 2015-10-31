module Raskell.Evaluator.ProgramContext (
  ProgramContext (..)
, emptyProgramContext
) where

import qualified Data.Map as M
import qualified Raskell.RubyObject as Rb

data ProgramContext = ProgramContext { _contextMap :: (M.Map String Rb.RbObject) } deriving (Show, Eq)

emptyProgramContext :: ProgramContext
emptyProgramContext = ProgramContext M.empty
