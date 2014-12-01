module Raskell.Parser.RubyGrammar
( var
, parens
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Control.Monad (void)
import Raskell.Parser.FunctionsAndTypesForParsing (regularParse, parseWithEof, parseWithLeftOver)
import Raskell.Parser.Lexemes
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

parens :: Parser Parenthesis
parens = do
   void $ char '('
   e <- many1 digit
   void $ char ')'
   return $ Parenthesis $ read e
