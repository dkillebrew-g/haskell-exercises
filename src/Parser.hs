module Parser where

import ClassyPrelude
import Data.Void (Void)
import LispVal
import Text.Megaparsec
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- | Consumes trailing white space, which includes line and block comments.
sc :: Parser ()
sc =
  L.space
    space1 -- (2)
    (L.skipLineComment "//") -- (3)
    (L.skipBlockComment "/*" "*/") -- (4)

-- | A wrapper for lexemes that consumes all trailing white space using `sc`.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | A parser that matches the given text (using Text.Megaparsec.Char.string
-- internally) and then consumes all trailing white space using `sc`.
symbol :: Text -> Parser Text
symbol = L.symbol sc
