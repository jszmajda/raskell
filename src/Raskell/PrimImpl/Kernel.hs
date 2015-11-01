module Raskell.PrimImpl.Kernel
(
  invokeMethodIO
) where

import Raskell.Evaluator.ProgramContext
import qualified Raskell.RubyObject as Rb

invokeMethodIO :: ProgramContext -> Rb.RbObject -> Rb.MethodName -> [Rb.RbObject] -> IO Rb.RbObject
invokeMethodIO ctxt _ "puts" args = mapM_ (putStrLn . show) args >> return Rb.Null
invokeMethodIO _ _ _ _ = undefined
