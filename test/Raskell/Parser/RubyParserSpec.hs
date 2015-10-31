module Raskell.Parser.RubyParserSpec where

import SpecHelper
import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Error
import Data.Either
import Text.Parsec.Prim (Stream, ParsecT)
import Raskell.Parser.FunctionsAndTypesForParsing (regularParse)
import Raskell.Parser.RubyParser
import Raskell.Parser.Whitespace (whitespace)
import qualified Raskell.ASTNodes as AST
import Raskell.Parser.ParserSpecsHelpers

veryBasicParsing :: Spec
veryBasicParsing = do
  numericParsing
  rubyTokenParsing
  parensParsing
  binopParsing
  exprParsing

numericParsing :: Spec
numericParsing =
  describe "numeric parsing" $ do
    it "parses a simple number" $
      fullParse numeric "1" `shouldBe` AST.Int 1
    it "parses a simple number with multiple numbers" $
      fullParse numeric "123" `shouldBe` AST.Int 123
    it "parses a simple number with underscores" $
      fullParse numeric "1_2_3" `shouldBe` AST.Int 123
    it "parses a simple float" $
      fullParse numeric "1.23" `shouldBe` AST.Float 1.23
    it "doesn't parse a simple float with no leading number" $
      regularParse numeric ".23" `shouldSatisfy` isLeft
    it "parses a simple float with underscores" $
      fullParse numeric "1_2.23" `shouldBe` AST.Float 12.23
    it "parses a simple float with underscores on the right" $
      fullParse numeric "1_2.2_3" `shouldBe` AST.Float 12.23
    -- wat
    it "parses rubyTokens with upcase innards" $
      fullParse rubyToken "abCd" `shouldBe` AST.Token "abCd" []
    it "does not parse a rubyToken starting with a digit" $
      regularParse rubyToken "1abc" `shouldSatisfy` isLeft

rubyTokenParsing :: Spec
rubyTokenParsing =
  describe "rubyToken parsing" $ do
    it "parses an alpha rubyToken" $
      fullParse rubyToken "abcd" `shouldBe` AST.Token "abcd" []
    it "parses rubyTokens with upcase innards" $
      fullParse rubyToken "abCd" `shouldBe` AST.Token "abCd" []
    it "does not parse a rubyToken starting with a digit" $
      regularParse rubyToken "1abc" `shouldSatisfy` isLeft
    it "parses a rubyToken with a leading _" $
      fullParse rubyToken "_abc" `shouldBe` AST.Token "_abc" []
    it "parses a rubyToken with a leading __" $
      fullParse rubyToken "__abc" `shouldBe` AST.Token "__abc" []

parensParsing :: Spec
parensParsing =
  describe "parens parsing" $ do
    it "parses a number inside parens" $
      fullParse parens "(1)" `shouldBe` AST.Parens (AST.Int 1)
    it "parses a number inside parens with leading whitespace" $
      fullParse parens "( 1)" `shouldBe` AST.Parens (AST.Int 1)
    it "parses a number inside parens with trailing whitespace" $
      fullParse parens "(1 )" `shouldBe` AST.Parens (AST.Int 1)
    it "parses a number inside parens with whitespace" $
      fullParse parens "( 1 )" `shouldBe` AST.Parens (AST.Int 1)
    it "parses a number inside parens following whitespace" $
      fullParse parens "(1) " `shouldBe` AST.Parens (AST.Int 1)
    it "parses a long number inside parens" $
      fullParse parens "(1234)" `shouldBe` AST.Parens (AST.Int 1234)
    it "TODO parses blank parens" $ -- TODO FIXME
      -- regularParse parens "()" `shouldBe` AST.Parens ?
      regularParse parens "()" `shouldSatisfy` isLeft

binopParsing :: Spec
binopParsing =
  describe "parsing plus" $ do
    it "parses 2 + 2" $
      fullParse add "2+2" `shouldBe` AST.BPlus (AST.Int 2) (AST.Int 2)
    it "parses 2 + 2 with whitespace" $
      fullParse add " 2 + 2 " `shouldBe` AST.BPlus (AST.Int 2) (AST.Int 2)
    it "parses 2.0 + 2.1" $
      fullParse add "2.0+2.1" `shouldBe` AST.BPlus (AST.Float 2.0) (AST.Float 2.1)

exprParsing :: Spec
exprParsing =
  describe "expression parsing" $ do
    it "parses 2 + 2" $
      fullParse expr "2 + 2" `shouldBe` AST.BPlus (AST.Int 2) (AST.Int 2)
    it "parses parens with add" $
      fullParse expr "(2 +3)" `shouldBe` AST.Parens (AST.BPlus (AST.Int 2) (AST.Int 3))
    it "parses float with add" $
      fullParse expr "(2.0 +3)" `shouldBe` AST.Parens (AST.BPlus (AST.Float 2.0) (AST.Int 3))
    it "parses add with rubyToken" $
      fullParse expr "(2.0 + a)" `shouldBe` AST.Parens (AST.BPlus (AST.Float 2.0) (AST.Token "a" []))
    it "parses a simple command with a num" $
      fullParse expr "print 4" `shouldBe` AST.Token "print" [AST.Int 4]
    it "parses a simple command with a string" $
      fullParse expr "print \"Hello World\"" `shouldBe` AST.Token "print" [AST.String "Hello World"]

-- varAssignParsing :: Spec
-- varAssignParsing =
--   describe "var assign parsing" $ do

main :: IO ()
main = hspec spec
spec :: Spec
spec = veryBasicParsing
