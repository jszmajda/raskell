module Raskell.Parser.RubyGrammar
( var
, parens
, add
, numeric
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Control.Monad (void)
import Raskell.Parser.FunctionsAndTypesForParsing (regularParse, parseWithEof, parseWithLeftOver)
import Raskell.Parser.ASTNodes
import Raskell.Parser.Whitespace (lexeme)
import Data.Char

{-
  stmt  ::= nop | var = expr
  expr  ::= var | const | unop expr | expr duop expr
  var   ::= rubyword
  const ::= rubyword
  unop  ::= rubyword
  rubyword ::= letter { letter | digit | underscore }
-}

numeric :: Parser Expr
numeric = try numericFloat <|> numericInt

numericInt :: Parser Expr
numericInt = do
    x <- lexeme $ many1 number
    return $ Int $ read (filtered x)
  where
    filtered = filter (/= '_')
    number = digit <|> char '_'

numericFloat :: Parser Expr
numericFloat = do
    x <- lexeme $ many1 number
    void $ lexeme $ char '.'
    y <- lexeme $ many1 number
    return $ Float $ read $ filtered (x ++ "." ++ y)
  where
    filtered = filter (/= '_')
    number = digit <|> char '_'  -- TODO should not consume . unless digit is after

var :: Parser Expr
var = do
    fc <- lexeme firstChar
    rest <- many varChars
    return $ Var (fc : rest)
  where
    firstChar = oneOf lcLetters
    varChars  = oneOf (lcLetters ++ ucLetters ++ numbers)
    lcLetters   = '_' : ['a'..'z']
    ucLetters   = ['A'..'Z']
    numbers     = ['0'..'9']

parens :: Parser Expr
parens = do
   void $ lexeme $ char '('
   e <- numeric
   void $ lexeme $ char ')'
   return $ Parens e

add :: Parser Expr
add = do
    lhs <- lexeme numeric
    void $ lexeme $ char '+'
    rhs <- lexeme numeric
    return $ BPlus lhs rhs
