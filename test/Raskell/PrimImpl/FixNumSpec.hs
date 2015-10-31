module Raskell.PrimImpl.FixNumSpec where

import SpecHelper
import qualified Raskell.RubyObject as Rb
import qualified Raskell.PrimImpl.FixNum as Impl
import Raskell.Evaluator.ProgramContext (emptyProgramContext)

plusSpec :: Spec
plusSpec =
  describe "adding two fixnums" $ do
    it "adds 2 and 3" $ do
      Impl.invokeMethod emptyProgramContext (Rb.FixNum 2) "+" [(Rb.FixNum 3)] `shouldBe` (Rb.FixNum 5)

main :: IO ()
main = hspec spec
spec :: Spec
spec = plusSpec
