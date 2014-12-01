module Raskell.Parser.RubyGrammar
( var
, parens
, add
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Control.Monad (void)
import Raskell.Parser.FunctionsAndTypesForParsing (regularParse, parseWithEof, parseWithLeftOver)
import Raskell.Parser.ASTNodes
import Data.Char

{-
  stmt  ::= nop | var = expr
  expr  ::= var | const | unop expr | expr duop expr
  var   ::= rubyword
  const ::= rubyword
  unop  ::= rubyword
  rubyword ::= letter { letter | digit | underscore }
-}

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \t"

-- lexeme :: Parser a -> Parser b
-- lexeme p = do
--   x <- p
--   whitespace
--   return x

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

add :: Parser BPlus
add = do
    lhs <- many1 number
    whitespace
    void $ char '+'
    whitespace
    rhs <- many1 number
    return $ BPlus (read lhs) (read rhs)
  where
    number = digit <|> char '.' -- TODO should not consume . unless digit is after
