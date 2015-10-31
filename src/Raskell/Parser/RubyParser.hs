module Raskell.Parser.RubyParser
( rubyToken
, parens
, add
, numeric
, expr
, parseRubySource
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Control.Monad (void)
import Raskell.Parser.FunctionsAndTypesForParsing (regularParse, parseWithEof, parseWithLeftOver)
import Raskell.Parser.RbStrings
import qualified Raskell.ASTNodes as AST
import Raskell.Parser.Whitespace (lexeme, whitespace)
import Data.Char

{-
  stmt  ::= nop | rubyToken = expr
  expr  ::= rubyToken | const | unop expr | expr duop expr
  rubyToken   ::= rubyword
  const ::= rubyword
  unop  ::= rubyword
  rubyword ::= letter { letter | digit | underscore }
-}

expr :: Parser AST.Expr
expr = term' `chainl1` plus
  where
    plus = do
      void $ lexeme $ char '+'
      return AST.BPlus
    term' = term expr

term :: Parser AST.Expr -> Parser AST.Expr
term expr' =  numeric
          <|> rbString
          <|> rubyToken
          <|> parens' expr'

numeric :: Parser AST.Expr
numeric = try numericFloat <|> numericInt

numericInt :: Parser AST.Expr
numericInt = do
  x <- rubyNumber
  return $ AST.Int $ read x

numericFloat :: Parser AST.Expr
numericFloat = do
  x <- rubyNumber
  void $ lexeme $ char '.'
  y <- rubyNumber
  return $ AST.Float $ read (x ++ "." ++ y)

rubyNumber :: Parser String
rubyNumber = do
    n <- lexeme $ many1 nums
    return $ filter (/= '_') n
  where
    nums = digit <|> char '_'


rubyToken :: Parser AST.Expr
rubyToken = do
    fc <- lexeme firstChar
    rest <- lexeme $ many varChars
    args <- many expr
    return $ AST.Token (fc : rest) args
  where
    firstChar = oneOf lcLetters
    varChars  = oneOf (lcLetters ++ ucLetters ++ numbers)
    lcLetters   = '_' : ['a'..'z']
    ucLetters   = ['A'..'Z']
    numbers     = ['0'..'9']

parens :: Parser AST.Expr
parens = parens' expr
parens' :: Parser AST.Expr -> Parser AST.Expr
parens' expr' = do
   void $ lexeme $ char '('
   e <- expr'
   void $ lexeme $ char ')'
   return $ AST.Parens e

add :: Parser AST.Expr
add = do
    lhs <- lexeme (term expr)
    void $ lexeme $ char '+'
    rhs <- lexeme expr
    return $ AST.BPlus lhs rhs

parseRubySource :: String -> AST.Expr
parseRubySource s = unbox $ parse (whitespace >> expr) "" s
  where
    unbox :: Either ParseError a -> a
    unbox (Left _) = error "failed"
    unbox (Right a) = a
