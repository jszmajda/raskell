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
      runExpr [(AST.Int 4)] emptyWorld `shouldBe` ([], emptyWorld)

evalSpec :: Spec
evalSpec =
  describe "evaluating a simple expression into an object" $ do
    it "converts AST.Int 4 to RbFixNum 4" $
      evalExpr (AST.Int 4) emptyWorld `shouldBe` Rb.FixNum 4
    it "converts AST.Float 4.0 to RbFloat 4.0" $
      evalExpr (AST.Float 4.0) emptyWorld `shouldBe` Rb.Float 4.0

assignmentSpec :: Spec
assignmentSpec =
  describe "assignment" $ do
    it "sets a key with a value" $ do
      runExpr [(AST.VarAssign "x" (AST.Int 4))] emptyWorld `shouldBe` ([], World (M.fromList [("x", (Rb.FixNum 4))]))

main :: IO ()
main = hspec spec
spec :: Spec
spec = do
  noopSpec
  evalSpec
  assignmentSpec
