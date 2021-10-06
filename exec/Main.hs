module Main (main) where

import ClassyPrelude
import Control.Monad.Trans.Maybe

-- MTL (Monad transformer library)?

-- Some useful links:
-- https://www.stackage.org/lts-18.10/hoogle

-- Do a stackage search for MaybeT
-- skim/read the docs

-- Returns (Just x/2) when x is evenly divisible by 2.
div2 :: Int -> Maybe Int
div2 x = undefined

mul2 :: Int -> Int
mul2 = (* 2)

-- The monad transformer you will use will be `MaybeT IO a`
-- Do all of these in one `do` block:

-- read a value from the console (getLine)
-- parse it to an integer (could fail)
-- div2
-- mul2
-- print it back out
-- return the result of mul2

main :: IO ()
main = do
  putStrLn "Hello world"
