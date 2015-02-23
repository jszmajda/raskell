module Raskell.Parser.ParserSpecsHelpers
(
  fullParse
) where

import SpecHelper
import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Error
import Raskell.Parser.Whitespace (whitespace)

fullParse :: Parser a -> String -> a
fullParse p = unbox . parse (whitespace >> p) ""
  where
    unbox :: Either ParseError a -> a
    unbox (Left _) = error "failed"
    unbox (Right a) = a

