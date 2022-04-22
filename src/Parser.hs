module Parser where

import ClassyPrelude hiding (many, some)
import Data.Void (Void)
import LispVal
import Text.Megaparsec
import Text.Megaparsec.Char
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

-- Follows https://www.scheme.com/tspl2d/grammar.html section Identifiers.
identifier :: Parser Text
identifier =
  let initial :: Parser Char
      initial =
        letterChar
          <|> oneOf validChars
            -- ['!', '$', '%', '&', '*', '/', ':', '<', '=', '>', '?', '~', '_', '^']
            -- oneOf "!$"
      validChars :: [Char]
      validChars = "$!"
      -- :: Text
      -- :: ByteString
      subsequent :: Parser Char
      subsequent = initial <|> digitChar <|> oneOf ['.', '+', '-']
      unpacked :: Parser [Char]
      unpacked = (:) <$> initial <*> many subsequent
      someIdentifier :: Parser Text
      someIdentifier = pack <$> unpacked
      exceptionalIdentifier :: Parser Text
      exceptionalIdentifier = string "+" <|> string "-" <|> string "..."
   in someIdentifier <|> exceptionalIdentifier

parseAtom :: Parser LispVal
parseAtom = lexeme (Atom <$> identifier)

parseText :: Parser LispVal
parseText = do
  p <- char '"' *> takeWhile1P (Just "Text") (/= '"') <* char '"'
  lexeme (pure (String p))

fromJust :: Maybe a -> a
fromJust Nothing = error "fromJust"
fromJust (Just x) = x

parseNumber :: Parser LispVal
parseNumber = do
  negativeMay <- optional (char '-')
  digits <- some digitChar
  let signedDigits = maybe digits (: digits) negativeMay
  lexeme . pure . Number . fromJust . readMay $ signedDigits
