module Raskell.Parser.Whitespace
( whitespace
, lexeme
) where

import Text.Parsec (many)
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Control.Monad (void)
import Control.Applicative ((<*))

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace
