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
data Parser -- TODO
-- Note that depending on your definition above, some of the mentions of Parser,
-- below, must change.

-- | Parses this string using this parser.
--
-- The type of `undefined` depends on how you chose to define `Parser`, so you
-- must fill it in.
runParser :: Parser -> String -> undefined
runParser = undefined

-- | Creates a parser for a single character
char :: Char -> Parser
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
-- successful parse. If all fail, it fails.
oneOf :: [Parser] -> Parser
oneOf = undefined

-- > theLetterC = assumeSuccess (runParser (oneOf [char 'a', char 'b', char 'c']) "c")
-- > theLetterC == 'c'
-- > True

-- | Given a function and a parser, returns a parser that: Runs the parser, and
-- if it was successful, apply the function to the parsed value.
-- modifyResult :: (a -> b) -> Parser ? -> Parser ?
-- modifyResult = undefined

-- | Given a value, returns a parser that always succeeds and always returns the
-- given value.
-- makeParserThatAlwaysReturns :: a -> Parser ?
-- makeParserThatAlwaysReturns = undefined

-- | Given two values, returns a parser that runs the first then the second
-- parser (and the returned parser fails ASAP if either given parser fail).
-- Additionally, the first parser returns a function, while the second parser
-- returns a value that can be passed to the function.
-- sequenceParsers :: Parser ? -> Parser ? -> Parser ?
-- sequenceParsers = undefined




-- Implement Functor for your Parser type

-- Implement Applicative for your Parser type

main :: IO ()
main = do
  undefined
