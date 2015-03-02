module Raskell.Evaluator.RunnerSpec where

import SpecHelper
import Raskell.ASTNodes
import Raskell.RubyObject
import Raskell.Evaluator.Runner
import qualified Data.Map as M

noopSpec :: Spec
noopSpec =
  describe "evaluating noops against a world" $ do
    it "does nothing for an int" $
      runExpr [(RbInt 4)] emptyWorld `shouldBe` ([], emptyWorld)

assignmentSpec :: Spec
assignmentSpec =
  describe "assignment" $ do
    it "sets a key with a value" $ do
      runExpr [(VarAssign "x" (RbInt 4))] emptyWorld `shouldBe` ([], World (M.fromList [("x", (rubyNumber 4))]))

main :: IO ()
main = hspec spec
spec :: Spec
spec = noopSpec
