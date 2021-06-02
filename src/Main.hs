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

import Prelude

-- Fill in all TODOs and `undefined`s.

-- Define a data type that represents a parser (in the parser combinator style).
-- A parser combinator is a function that takes a string and returns one of:
-- * An error
-- * The value that was parsed, and the rest of the string (which may be used by
--   subsequent parsers)
-- Hint: a parser can produce a value of any type...
data Parser a = Parser (String -> Maybe (a, String))

-- TODO
-- Note that depending on your definition above, some of the mentions of Parser,
-- below, must change.

-- | Parses this string using this parser.
--
-- The type of `undefined` depends on how you chose to define `Parser`, so you
-- must fill it in.
runParser :: Parser a -> String -> Maybe (a, String)
runParser = undefined

-- | Creates a parser for a single character
char :: Char -> Parser Char
char = undefined

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
oneOf = undefined

-- > theLetterC = assumeSuccess (runParser (oneOf [char 'a', char 'b', char 'c']) "c")
-- > theLetterC == 'c'
-- > True

-- | Given a function and a parser, returns a parser that: Runs the parser, and
-- if it was successful, apply the function to the parsed value.
modifyResult :: (a -> b) -> Parser a -> Parser b
modifyResult = undefined

-- | Given a value, returns a parser that always succeeds and always returns the
-- given value.
makeParserThatAlwaysReturns :: a -> Parser a
makeParserThatAlwaysReturns = undefined

-- | Given two values:
-- * A parser that creates a function of one argument.
-- * A parser that creates a value which can be passed to the function
--
-- `sequenceParsers` returns a parser that runs the first, then the second,
--   then applies the value to the function.
sequenceParsers :: Parser (a -> b) -> Parser a -> Parser b
sequenceParsers = undefined

-- Implement Functor for your Parser type

-- Implement Applicative for your Parser type

-- Let's make some more combinators, which we'll use in the exercise that
-- follows.

-- | A list with at least 1 element.
--
-- Note: this is an example of "Make illegal states unrepresentable". We've used
-- the type system to make empty lists impossible. The compiler enforces our
-- desired semantics.
data NonNull a = NonNull a [a]

-- | Parses at least one, and possibly more, of something.
some :: Parser a -> Parser (NonNull a)
some pe@(Parser parseElement) =
  let
      parseFirst = Parser (\s ->
        case parseElement s of
            Nothing -> Nothing
            Just (x, s') -> Just (NonNull x, s') )
            -- A more verbose version of the above:
            -- Just (x, s') -> Just (\xs -> NonNull x xs, s') )
  in  sequenceParsers parseFirst (many pe)

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

number :: Parser Number
number = undefined

main :: IO ()
main = do
  undefined
