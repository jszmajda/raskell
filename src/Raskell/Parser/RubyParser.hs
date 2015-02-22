module Raskell.Parser.RubyParser
( rubyToken
, parens
, add
, numeric
, expr
, basicDblQuotSring
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
  stmt  ::= nop | rubyToken = expr
  expr  ::= rubyToken | const | unop expr | expr duop expr
  rubyToken   ::= rubyword
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
term expr' =  numeric
          <|> basicDblQuotSring
          <|> rubyToken
          <|> parens' expr'

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

basicDblQuotSring :: Parser Expr
basicDblQuotSring = do
    lexeme $ char '"'
    body <- many character
    char '"'
    return $ RbString $ concat body
  where
    escape :: Parser String
    escape = do
      d <- char '\\'
      c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
      return [d, c]

    nonEscape :: Parser Char
    nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

    character :: Parser String
    character = fmap return nonEscape <|> escape

rubyToken :: Parser Expr
rubyToken = do
    fc <- lexeme firstChar
    rest <- lexeme $ many varChars
    args <- many expr
    return $ RubyToken (fc : rest) args
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
