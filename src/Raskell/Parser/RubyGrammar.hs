module Raskell.Parser.RubyGrammar where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Raskell.Parser.FunctionsAndTypesForParsing (regularParse, parseWithEof, parseWithLeftOver)
import Data.Char

{-
  stmt  ::= nop | var = expr
  expr  ::= var | const | unop expr | expr duop expr
  var   ::= rubyword
  const ::= rubyword
  unop  ::= rubyword
  rubyword ::= letter { letter | digit | underscore }
-}

var :: Parser String
var = do
    fc <- firstChar
    rest <- many varChars
    return $ fc : rest
  where
    firstChar = oneOf lcLetters
    varChars  = oneOf (lcLetters ++ ucLetters ++ numbers)
    lcLetters   = '_' : ['a'..'z']
    ucLetters   = ['A'..'Z']
    numbers     = ['0'..'9']
