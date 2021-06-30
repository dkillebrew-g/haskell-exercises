{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

-- Some useful links:
-- https://www.stackage.org/lts-14.27/hoogle

import Data.Char
import Prelude

-- Fill in all TODOs and `undefined`s.

-- Define a data type that represents a parser (in the parser combinator style).
-- A parser combinator is a function that takes a string and returns one of:
-- * An error
-- * The value that was parsed, and the rest of the string (which may be used by
--   subsequent parsers)
-- Hint: a parser can produce a value of any type...
newtype Parser a = Parser (String -> Maybe (a, String))

-- TODO
-- Note that depending on your definition above, some of the mentions of Parser,
-- below, must change.

-- | Parses this string using this parser.
--
-- The type of `undefined` depends on how you chose to define `Parser`, so you
-- must fill it in.
runParser :: Parser a -> String -> Maybe (a, String)
runParser (Parser fun) input = fun input

-- | Creates a parser for a single character
char :: Char -> Parser Char
char c = Parser $ \s ->
  case s of
    (x : xs) -> if c == x then Just (x, xs) else Nothing
    [] -> Nothing

-- range:: Char -> Char -> Parser Char
-- range = undefined

-- lowercaseParser = range 'a' 'z'

-- | This function fatal errors if the parsing failed, and is only implemented
-- as a convenience method used below. I.e., this is bad error handling.
-- assumeSuccess :: YourParserResultType a -> a
-- assumeSuccess = undefined

-- Test your char parser. For example:
-- theLetterA = assumeSuccess (runParser (char 'a') "a")
-- > theLetterA == 'a'
-- > True

-- | Returns a parser that: runs the parsers in order, stopping at the first
-- successful parse. If all fail, it fails. The subparsers should not interfere
-- with each other; the original string should be passed to each subparser in
-- turn.
oneOf :: [Parser a] -> Parser a
oneOf parsers = Parser $ \s ->
  let go (p : ps) =
        let result = runParser p s
         in case result of
              Nothing -> go ps
              Just _ -> result
      go [] = Nothing
   in go parsers

-- > theLetterC = assumeSuccess (runParser (oneOf [char 'a', char 'b', char 'c']) "c")
-- > theLetterC == 'c'
-- > True

-- | Given a function and a parser, returns a parser that: Runs the parser, and
-- if it was successful, apply the function to the parsed value.
modifyResult :: (a -> b) -> Parser a -> Parser b
modifyResult func parser = Parser $ \s -> do
  (val, result) <- runParser parser s
  pure (func val, result)

-- | Given a value, returns a parser that always succeeds and always returns the
-- given value.
makeParserThatAlwaysReturns :: a -> Parser a
makeParserThatAlwaysReturns value = Parser $ \s -> Just (value, s)

-- | Given two values:
-- * A parser that creates a function of one argument.
-- * A parser that creates a value which can be passed to the function
--
-- `sequenceParsers` returns a parser that runs the first, then the second,
--   then applies the value to the function.
sequenceParsers :: Parser (a -> b) -> Parser a -> Parser b
sequenceParsers firstParser secondParser = Parser $ \s -> do
  (fun, result1) <- runParser firstParser s
  (val, result2) <- runParser secondParser result1
  pure $ (fun val, result2)

-- Implement Functor for your Parser type
instance Functor Parser where
  fmap = modifyResult

-- Implement Applicative for your Parser type
instance Applicative Parser where
  pure :: a -> Parser a
  pure = makeParserThatAlwaysReturns

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) = sequenceParsers

-- Let's make some more combinators, which we'll use in the exercise that
-- follows.

-- | A list with at least 1 element.
--
-- Note: this is an example of "Make illegal states unrepresentable". We've used
-- the type system to make empty lists impossible. The compiler enforces our
-- desired semantics.
data NonNull a = NonNull a [a]

digit :: Parser Int
digit =
  -- fmap
  --   digitToInt
  --   ( oneOf -- :: Parser Char
  --       [ char '0',
  --         char '1',
  --         char '2',
  --         char '3',
  --         char '4',
  --         char '5',
  --         char '6',
  --         char '7',
  --         char '8',
  --         char '9'
  --       ]
  --   )
  oneOf
    [ char '0' *> pure 0,
      char '1' *> pure 1,
      char '2' *> pure 2,
      char '3' *> pure 3,
      char '4' *> pure 4,
      char '5' *> pure 5,
      char '6' *> pure 6,
      char '7' *> pure 7,
      char '8' *> pure 8,
      char '9' *> pure 9
    ]

-- | Parses at least one, and possibly more, of something.
some :: Parser a -> Parser (NonNull a)
some pe@(Parser parseElement) =
  let parseFirst =
        Parser
          ( \s ->
              case parseElement s of
                Nothing -> Nothing
                Just (x, s') -> Just (NonNull x, s')
          )
   in -- A more verbose version of the above:
      -- Just (x, s') -> Just (\xs -> NonNull x xs, s') )
      sequenceParsers parseFirst (many pe)

-- | Parses zero or more of something.
--
-- Returns an empty list when it can't parse even one element, and does not
-- consume any of the input.
many :: Parser a -> Parser [a]
many (Parser parseElement) =
  let parseList s =
        case parseElement s of
          Nothing -> Just ([], s)
          Just (x, s') -> (\(xs, s'') -> (x : xs, s'')) <$> parseList s'
   in Parser parseList

-- Now let's create a parser for numbers. Let's parse two types of numbers:
-- non-negative integers (e.g. parse 0 and parse 3, but don't parse -3), and
-- decimals with fractional digits (e.g. 3.141). We want to differentiate
-- between the two. Feel free to ask me how to handle any edge cases, or do
-- whatever you think reasonable.

-- 1) Write a data type that represents these two kinds of numbers (and
--    differentiates between them, hint: sum type).
data Number

-- 2) Write a parser for non-negative integers. E.g "3"
-- 3) Write a parser for decimals with fractional digits. E.g. "3.14"
-- 4) Write a parser for Number by combining the above. Note that the longest
--    matching prefix should be used, so "3.14" should be parsed as a decimal
--    with fractional digits, instead of a non-negative integer.

-- | Parses e.g. "3.14" into ([3], [1,4])
floatParts :: Parser (NonNull Int, [Int])
floatParts =
  -- let
  --     pureTuple :: Parser (a -> b -> (a, b))
  --     pureTuple = pure (,)

  --     leftOfDecimal = some digit

  --     step1 ::Parser (b0 -> (NonNull Int, b0))
  --     step1 = pureTuple <*> leftOfDecimal

  --     decimalAndRightOfIt :: Parser [Int]
  --     decimalAndRightOfIt = (char '.') *> many digit
  -- in

    -- fmap :: (a -> b) -> f a -> f b
    -- (,) <$> (some digit <* char '.') <*> many digit
    -- (,) `fmap` (some digit <* char '.') <*> many digit
    pure (,) <*> (some digit <* char '.') <*> many digit
    -- pure (,) <*> some digit <*> (char '.' *> many digit)

    -- ((pure (,) <*> some digit) <*> many digit) *> many digit

-- floating :: Parser Float
-- floating = undefined <$> floatParts

number :: Parser Number
number = undefined

-- Exercise:
-- Create mathematical expression parser & evaluator.
-- three = evaluate (assumeSuccess $ runParser parseMathExpression "1 + 2")
-- three == 3 -- is True

parseBinaryOperator :: Parser BinaryOperator
parseBinaryOperator =
  oneOf [char '-' *> pure Sub, char '+' *> pure Add]

parseInteger :: Parser Integer
parseInteger =
  oneOf
    [ char '0' *> pure 0,
      char '1' *> pure 1,
      char '2' *> pure 2,
      char '3' *> pure 3,
      char '4' *> pure 4,
      char '5' *> pure 5,
      char '6' *> pure 6,
      char '7' *> pure 7,
      char '8' *> pure 8
    ]

-- EBNF:
-- Literal = [0-9]+
-- BinaryOperator = '+' | '-'
-- MathExpression = Literal | MathExpression BinaryOperator MathExpression
data BinaryOperator = Add | Sub
  deriving (Show)

data MathExpression
  = Literal Integer
  | BinaryExpression MathExpression BinaryOperator MathExpression
  deriving (Show)

parseMathExpression :: Parser MathExpression
parseMathExpression =
  let parseLiteral :: Parser MathExpression
      parseLiteral = pure Literal <*> parseInteger
   in oneOf
        [ -- Avoid infinite loop: parseMathExpression cannot be leftmost.
          pure BinaryExpression <*> parseLiteral <*> parseBinaryOperator <*> parseMathExpression,
          parseLiteral
        ]

evaluate :: MathExpression -> Int
evaluate = undefined

-- Data.Map

-- TemplateHaskell
data Environment = Environment Map
  -- deriving (ToJson, FromJson)

-- Generics

-- three = evaluate (Environment {'x' : 2}) (assumeSuccess $ runParser parseMathExpression "1 + x")
-- three = evaluate (Environment (fromList [('x', 2)])) (assumeSuccess $ runParser parseMathExpression "1 + x")

main :: IO ()
main = do
  undefined
