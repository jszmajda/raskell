module Raskell.Parser.RbStringsSpec where

import SpecHelper
import Raskell.Parser.ParserSpecsHelpers
import Raskell.ASTNodes
import Raskell.Parser.RbStrings

basicDblQuotedStringParsing :: Spec
basicDblQuotedStringParsing =
  describe "parsing basic double-quoted strings" $ do
    it "parse \"hello\"" $
      fullParse rbString "\"hello\"" `shouldBe` RbString "hello"

main :: IO ()
main = hspec spec
spec :: Spec
spec = basicDblQuotedStringParsing
