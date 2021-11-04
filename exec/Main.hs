{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import ClassyPrelude

import Control.Monad.Except
import Control.Monad.Reader

-- Some useful links:
-- https://www.stackage.org/lts-18.10/hoogle

-- this is in the branch 'exercise/monad_transformer':

-- Returns (Just x/2) when x is evenly divisible by 2.
div2 :: Int -> Maybe Int
div2 x = undefined

mul2 :: Int -> Int
mul2 = (* 2)

getLineJustIo :: IO Text
getLineJustIo = getLine

printJustIo :: Show a => a -> IO ()
printJustIo = print

-- parse it to an integer (could fail)
-- div2
-- mul2
-- return the result of mul2
parseAndDiv :: (MonadError () m, MonadReader Text m) => m Int
parseAndDiv = do
   line <- ask
   _ <- when undefined (throwError ())
   pure undefined

main :: IO ()
main = do
  line <- getLine

  let
    --   bar :: Int
    --   bar = runReader parseAndDiv line
    --   bar :: Maybe Int
    --   bar = runReader parseAndDiv line

      bar :: Maybe Int
      bar = runReader parseAndDiv line


  -- We'll call parseAndDiv, passing it the line we got via getLine
  -- if it succeeds and returns an int, then print the int.

  pure ()
