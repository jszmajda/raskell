module Raskell.Parser.RbStrings
(rbString) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Raskell.Parser.ASTNodes
import Raskell.Parser.Whitespace (lexeme)

rbString :: Parser Expr
rbString = basicDblQuotSring

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
