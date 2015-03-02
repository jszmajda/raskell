module Raskell.Parser.RbStringsSpec where

import SpecHelper
import Raskell.Parser.ParserSpecsHelpers
import qualified Raskell.ASTNodes as AST
import Raskell.Parser.RbStrings

basicDblQuotedStringParsing :: Spec
basicDblQuotedStringParsing =
  describe "parsing basic double-quoted strings" $
    it "parse \"hello\"" $
      fullParse rbString "\"hello\"" `shouldBe` AST.String "hello"

main :: IO ()
main = hspec spec
spec :: Spec
spec = basicDblQuotedStringParsing
