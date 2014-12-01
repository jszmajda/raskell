module Raskell.Parser.RubyGrammarSpec where

import SpecHelper

spec :: Spec
spec = do
  describe "something" $ do
    it "something" $ do
      4 `shouldBe` 4

main :: IO ()
main = hspec spec
