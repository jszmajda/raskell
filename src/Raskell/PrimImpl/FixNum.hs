module Raskell.PrimImpl.FixNum (
  invokeMethod
) where

import Raskell.Evaluator.ProgramContext
import qualified Raskell.RubyObject as Rb

invokeMethod :: ProgramContext -> Rb.RbObject -> Rb.MethodName -> [Rb.RbObject] -> Rb.RbObject
invokeMethod _ this@(Rb.FixNum _) "+" = invokePlus this
invokeMethod _ _ _ = undefined

invokePlus :: Rb.RbObject -> [Rb.RbObject] -> Rb.RbObject
invokePlus (Rb.FixNum this) (arg:_) = Rb.FixNum $ this + (val arg)
invokePlus _ _ = undefined

val :: Rb.RbObject -> Integer
val (Rb.FixNum a) = a
val _ = undefined
