{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

-- Some useful links:
-- https://www.stackage.org/lts-14.27/hoogle
-- https://www.stackage.org/haddock/lts-14.27/mtl-2.2.2/Control-Monad-Reader.html#g:2

import Control.Monad.Reader
import Data.Map (Map, fromList)
import Prelude

type IntToString = Map Int String

-- TODO: use {-# LANGUAGE OverloadedLists #-}
-- to make this more concise
mapZeroToTwo :: IntToString
mapZeroToTwo = fromList [(0, "zero"), (1, "one"), (2, "two")]

-- This type captures the possibility for error:
--    IntToString -> Int -> Maybe String
-- But we don't care about that right now.
lookupInt :: IntToString -> Int -> String
lookupInt = undefined

-- Adds the new key and value to the IntToString map
augmentStringMap :: IntToString -> Int -> String -> IntToString
augmentStringMap = undefined

lookupTwoInts :: IntToString -> (Int, Int) -> (String, String)
lookupTwoInts = undefined

type MapReader = Reader IntToString

-- Does the same as the version above, but this time using the Reader type.
lookupIntR :: Int -> MapReader String
lookupIntR = undefined

-- Does the same as the version above, but this time using the Reader type.
lookupTwoIntsR :: (Int, Int) -> MapReader (String, String)
lookupTwoIntsR = undefined

-- If the Int key is not in the map, add it to the map with value "unknown",
-- then do the lookup.
handleUndefinedLookup :: Int -> MapReader String
handleUndefinedLookup =
  -- Your implementation should use `Control.Monad.Reader.withReader`
  undefined

main :: IO ()
main = do
  undefined
