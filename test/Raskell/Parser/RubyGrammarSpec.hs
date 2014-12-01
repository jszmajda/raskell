module Raskell.Parser.RubyGrammarSpec where

import SpecHelper
import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Error
import Data.Either
import Text.Parsec.Prim (Stream, ParsecT)
import Raskell.Parser.FunctionsAndTypesForParsing (regularParse)
import Raskell.Parser.RubyGrammar

fullParse :: Parser a -> String -> a
fullParse p = unbox . parse p ""
  where
    unbox :: Either ParseError a -> a
    unbox (Left _) = error "failed"
    unbox (Right a) = a

veryBasicParsing :: Spec
veryBasicParsing = variableParsing

variableParsing :: Spec
variableParsing = do
  describe "variable parsing" $
    it "stuff" $ 4 `shouldBe` 4
  describe "variable parsing" $ do
    it "parses an alpha variable" $
      fullParse var "abcd" `shouldBe` "abcd"
    it "parses variables with upcase innards" $
      fullParse var "abCd" `shouldBe` "abCd"
    it "does not parse a variable starting with a digit" $
      regularParse var "1abc" `shouldSatisfy` isLeft

main :: IO ()
main = hspec spec

spec :: Spec
spec = veryBasicParsing
