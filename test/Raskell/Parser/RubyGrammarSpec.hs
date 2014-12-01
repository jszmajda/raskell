module Raskell.Parser.RubyGrammarSpec where

import SpecHelper
import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Error
import Data.Either
import Text.Parsec.Prim (Stream, ParsecT)
import Raskell.Parser.FunctionsAndTypesForParsing (regularParse)
import Raskell.Parser.RubyGrammar
import Raskell.Parser.Lexemes

fullParse :: Parser a -> String -> a
fullParse p = unbox . parse p ""
  where
    unbox :: Either ParseError a -> a
    unbox (Left _) = error "failed"
    unbox (Right a) = a

veryBasicParsing :: Spec
veryBasicParsing = do
  variableParsing
  parensParsing
  unopParsing

variableParsing :: Spec
variableParsing =
  describe "variable parsing" $ do
    it "parses an alpha variable" $
      fullParse var "abcd" `shouldBe` "abcd"
    it "parses variables with upcase innards" $
      fullParse var "abCd" `shouldBe` "abCd"
    it "does not parse a variable starting with a digit" $
      regularParse var "1abc" `shouldSatisfy` isLeft

parensParsing :: Spec
parensParsing =
  describe "parens parsing" $ do
    it "parses a number inside parens" $
      fullParse parens "(1)" `shouldBe` Parenthesis 1
    it "parses a long number inside parens" $
      fullParse parens "(1234)" `shouldBe` Parenthesis 1234
    it "TODO parses blank parens" $ -- TODO FIXME
      regularParse parens "()" `shouldSatisfy` isLeft

unopParsing :: Spec
unopParsing = do
  describe "parsing plus" $ do
    it "parses 2 + 2" $
      fullParse add "2+2" `shouldBe` UPlus 2 2
    it "parses 2 + 2 with whitespace" $
      fullParse add "2 + 2" `shouldBe` UPlus 2 2
    it "parses 2.0 + 2.1" $
      fullParse add "2.0+2.1" `shouldBe` UPlus 2.0 2.1

main :: IO ()
main = hspec spec

spec :: Spec
spec = veryBasicParsing
