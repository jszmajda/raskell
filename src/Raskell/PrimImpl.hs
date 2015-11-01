module Raskell.PrimImpl (
  invokeMethodIO
, invokeMethod
) where

import Raskell.Evaluator.ProgramContext
import qualified Raskell.RubyObject as Rb
import qualified Raskell.PrimImpl.FixNum as ImplFixNum
import qualified Raskell.PrimImpl.Kernel as ImplKernel
import Control.Monad

-- hmmm this should return an IO action but I'm not sure how yet
invokeMethodIO :: ProgramContext -> Rb.RbObject -> Rb.MethodName -> [Rb.RbObject] -> IO Rb.RbObject
invokeMethodIO ctxt k@(Rb.Kernel) method args = ImplKernel.invokeMethodIO ctxt k method args
invokeMethodIO _ _ _ _ = undefined

invokeMethod :: ProgramContext -> Rb.RbObject -> Rb.MethodName -> [Rb.RbObject] -> Rb.RbObject
invokeMethod ctxt fixnum@(Rb.FixNum _) = ImplFixNum.invokeMethod ctxt fixnum
invokeMethod _ _ = undefined
