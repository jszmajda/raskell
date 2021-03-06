module Raskell.Parser.RbStrings
(rbString) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import qualified Raskell.ASTNodes as AST
import Raskell.Parser.Whitespace (lexeme)

rbString :: Parser AST.Expr
rbString = basicDblQuotSring

escape :: Parser String
escape = do
  d <- char '\\'
  c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
  return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

basicDblQuotSring :: Parser AST.Expr
basicDblQuotSring = do
  lexeme $ char '"'
  body <- many character
  char '"'
  return $ AST.String $ concat body
