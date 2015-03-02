module Raskell.Evaluator.RunnerSpec where

import SpecHelper
import Raskell.ASTNodes
import Raskell.Evaluator.Runner

noopSpec :: Spec
noopSpec =
  describe "evaluating noops against a world" $ do
    it "does nothing for an int" $
      runExpr [(RbInt 4)] emptyWorld `shouldBe` emptyWorld

main :: IO ()
main = hspec spec
spec :: Spec
spec = noopSpec
