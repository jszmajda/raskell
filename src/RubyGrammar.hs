module RubyGrammar where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import FunctionsAndTypesForParsing (regularParse, parseWithEof, parseWithLeftOver)
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
    firstChar = oneOf letters
    varChars  = oneOf (letters ++ ['0'..'9'])
    letters   = '_' : ['a'..'z']
