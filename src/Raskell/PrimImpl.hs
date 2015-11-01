module Raskell.PrimImpl (
  invokeMethodIO
, invokeMethod
) where

import Raskell.Evaluator.ProgramContext
import qualified Raskell.RubyObject as Rb
import qualified Raskell.PrimImpl.FixNum as ImplFixNum
import Control.Monad

invokeMethodIO :: ProgramContext -> Rb.RbObject -> Rb.MethodName -> [Rb.RbObject] -> IO Rb.RbObject
invokeMethodIO ctxt k@(Rb.Kernel) method args = invokeKernelMethodIO ctxt k method args
invokeMethodIO _ o m _ = error $ "Error: no implementation of " ++ m ++ " for " ++ (show o)

invokeMethod :: ProgramContext -> Rb.RbObject -> Rb.MethodName -> [Rb.RbObject] -> Rb.RbObject
invokeMethod ctxt fixnum@(Rb.FixNum _) method args = ImplFixNum.invokeMethod ctxt fixnum method args
invokeMethod _ o m _ = error $ "Error: no implementation of " ++ m ++ " for " ++ (show o)

-- Kernel Impl --------------------------------------------------------
invokeKernelMethodIO :: ProgramContext -> Rb.RbObject -> Rb.MethodName -> [Rb.RbObject] -> IO Rb.RbObject
invokeKernelMethodIO ctxt _ "puts" args = do
  mapM_ (\a -> putStrLn . stringFromString $ invokeMethod ctxt a "to_s" []) args
  return Rb.Null
    where
      stringFromString (Rb.String str) = str

invokeKernelMethodIO _ o m _ = error $ "Error: no implementation of " ++ m ++ " for " ++ (show o)

