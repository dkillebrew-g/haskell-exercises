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

-- Reader is a function like this: (someData -> a)

newtype MyReader someData return = MyReader (someData -> return)

instance Functor  (MyReader someData) where
  fmap :: (a -> b) -> MyReader someData a -> MyReader someData b
  fmap = undefined

instance Applicative (MyReader someData) where
  pure :: a -> MyReader someData a
  pure somevalue = MyReader (\_ -> somevalue)
  -- or equivalently:
  -- pure somevalue = MyReader (const somevalue)
-- (<*>) :: Reader someData (a -> b) -> Reader someData a -> Reader someData b

-- Monad
-- (>>=) :: Reader someData a -> (a -> Reader someData b) -> Reader someData b

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
lookupIntR i =
  asks (\mp -> lookupInt mp i)
  -- do
  -- mp <- ask
  -- pure (lookupInt mp i)

-- Does the same as the version above, but this time using the Reader type.
lookupTwoIntsR :: (Int, Int) -> MapReader (String, String)
lookupTwoIntsR (i0, i1) = do
  s0 <- lookupIntR i0
  s1 <- lookupIntR i1
  pure (s0, s1)
-- lookupTwoIntsR (i0, i1) =
--   liftM2 (,) (lookupIntR i0) (lookupIntR i1)

lookupTwoIntsList :: [Int] -> MapReader [String]
lookupTwoIntsList = mapM lookupIntR

-- If the Int key is not in the map, add it to the map with value "unknown",
-- then do the lookup.
handleUndefinedLookup :: Int -> MapReader String
handleUndefinedLookup =
  -- Your implementation should use `Control.Monad.Reader.withReader`
  undefined

main :: IO ()
main = do
  undefined
