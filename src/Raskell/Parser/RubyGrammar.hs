module Raskell.Parser.RubyGrammar
( var
, parens
, add
, numeric
, expr
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

expr :: Parser Expr
expr = term' `chainl1` plus
  where
    plus = do
      void $ lexeme $ char '+'
      return BPlus
    term' = term expr

term :: Parser Expr -> Parser Expr
term expr' = numeric <|> var <|> parens' expr'

numeric :: Parser Expr
numeric = try numericFloat <|> numericInt

numericInt :: Parser Expr
numericInt = do
  x <- rubyNumber
  return $ Int $ read x

numericFloat :: Parser Expr
numericFloat = do
  x <- rubyNumber
  void $ lexeme $ char '.'
  y <- rubyNumber
  return $ Float $ read (x ++ "." ++ y)

rubyNumber :: Parser String
rubyNumber = do
    n <- lexeme $ many1 nums
    return $ filter (/= '_') n
  where
    nums = digit <|> char '_'

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
parens = parens' expr
parens' :: Parser Expr -> Parser Expr
parens' expr' = do
   void $ lexeme $ char '('
   e <- expr'
   void $ lexeme $ char ')'
   return $ Parens e

add :: Parser Expr
add = do
    lhs <- lexeme (term expr)
    void $ lexeme $ char '+'
    rhs <- lexeme expr
    return $ BPlus lhs rhs
