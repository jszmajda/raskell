module Raskell.Evaluator.RunnerSpec where

import SpecHelper
import qualified Raskell.ASTNodes as AST
import qualified Raskell.RubyObject as Rb
import Raskell.Evaluator.Runner
import qualified Data.Map as M

noopSpec :: Spec
noopSpec =
  describe "evaluating noops against a world" $ do
    it "does nothing for an int" $
      runExpr ([(AST.Int 4)], emptyProgramContext) `shouldBe` ([], emptyProgramContext)

evalSpec :: Spec
evalSpec =
  describe "evaluating a simple expression into an object" $ do
    it "converts AST.Int 4 to RbFixNum 4" $
      evalExpr (AST.Int 4) emptyProgramContext `shouldBe` Rb.FixNum 4
    it "converts AST.Float 4.0 to RbFloat 4.0" $
      evalExpr (AST.Float 4.0) emptyProgramContext `shouldBe` Rb.Float 4.0

assignmentSpec :: Spec
assignmentSpec =
  describe "assignment" $ do
    it "sets a key with a value" $ do
      runExpr ([(AST.VarAssign "x" (AST.Int 4))], emptyProgramContext) `shouldBe` ([], ProgramContext (M.fromList [("x", (Rb.FixNum 4))]))

bplusSpec :: Spec
bplusSpec =
  describe "evaluating addition" $ do
    it "evaluates 2 + 5 to be 5" $ do
      evalExpr (AST.BPlus (AST.Int 2) (AST.Int 5)) emptyProgramContext `shouldBe` Rb.FixNum 7

main :: IO ()
main = hspec spec
spec :: Spec
spec = do
  noopSpec
  evalSpec
  assignmentSpec
  bplusSpec
