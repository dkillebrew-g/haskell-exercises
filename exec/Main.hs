{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import ClassyPrelude
import Control.Monad.Trans.Maybe

-- MTL (Monad transformer library)?

-- Some useful links:
-- https://www.stackage.org/lts-18.10/hoogle

-- Do a stackage search for MaybeT
-- skim/read the docs

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

-- The monad transformer you will use will be `MaybeT IO a`
-- Do all of these in one `do` block:

-- read a value from the console (getLine)
-- parse it to an integer (could fail)
-- div2
-- mul2
-- print it back out (only if it parsed AND divided by 2 successfully)
-- return the result of mul2

-- lift :: (MonadTrans t, Monad m) => m a -> t m a

-- transformers	Control.Monad.Trans.Class
-- Lift a computation from the argument monad to the constructed monad.

-- readMay :: Read a => Text -> Maybe a

main :: IO ()
main = do
  line <- getLine
  let asInt :: Maybe Int
      asInt = readMay line
      divided_fmap = join (fmap div2 asInt) -- just for demonstration
      divided = asInt >>= div2
      multiplied = fmap mul2 divided
  case multiplied of
      Nothing -> pure ()
      Just x -> print x

  -- Now do things using the MaybeT monad transformer.
  -- the entire expression to the right of the `<-` must be of type `MaybeT IO a`
  let readAndDivMulPrint :: MaybeT IO Int
      readAndDivMulPrint = do
        pure undefined

  pure ()
