{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Main
  ( main,
  )
where

-- Some useful links:
-- https://www.stackage.org/lts-16.25/hoogle?q=State
-- https://www.stackage.org/haddock/lts-16.25/mtl-2.2.2/Control-Monad-State-Strict.html#g:2

import Control.Applicative
import Control.Monad.State.Strict (MonadState (get, put), State, execState, state)
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.QuickCheck.Checkers (EqProp (..), quickBatch)
import Test.QuickCheck.Classes (applicative, functor, monad)
import Prelude

mul2 :: Integer -> Integer
mul2 = (* 2)

add10 :: Integer -> Integer
add10 = (+ 10)

add10ThenMul2 :: Integer -> Integer
add10ThenMul2 = mul2 . add10

usingFmap :: Integer -> Integer
usingFmap = fmap mul2 add10

discardString :: String -> String
discardString x = ""

whatsTheType :: Integer -> Integer
whatsTheType = (+) <$> mul2 <*> add10

-- (+) <$> mul2 <*> add10
-- =
-- \x -> mul2 x + add10 x

newtype MyReader argType resultType = MyReader {runMyReader :: argType -> resultType}

instance Functor (MyReader argType) where
  fmap ::
    (resultA -> resultB) ->
    MyReader argType resultA ->
    MyReader argType resultB
  fmap aToB (MyReader argToResultA) =
    MyReader
      ( \x ->
          let foo = argToResultA x
           in aToB foo
      )

instance Applicative (MyReader argType) where
  pure :: result -> MyReader argType result
  pure someResult = MyReader (const someResult)

  -- pure someResult = MyReader (\_ -> someResult)

  (<*>) :: forall a b. MyReader argType (a -> b) -> MyReader argType a -> MyReader argType b
  (MyReader argToAToB) <*> fa =
    MyReader
      ( \someArgType ->
          let aToB = argToAToB someArgType
              MyReader argToB = fmap aToB fa
           in argToB someArgType
      )

add :: Int -> Int -> Int
add = undefined

-- std::bind
add2 = add 2

myFunc :: String -> Whatever

myFunc2 :: IO Whatever

data SystemBuilder

fooPlain :: SystemBuilder -> Integer -> String
fooPlain sb integer =
    let ..
        barssResult = barPlain sb
barPlain :: SystemBuilder -> Integer -> String

fooReader :: Integer -> Reader SystemBuilder String
fooReader = do
  barReader 10
barReader :: Integer -> Reader SystemBuilder String

main :: IO ()
main = do
  print $ (mul2 . add10) 3
  print $ (fmap mul2 add10) 3

  print $
    (runMyReader (fmap mul2 (MyReader add10))) 3

-- (partiallyAppliedFunction (fmap mul2 (MyReader add10))) 3

-- print $ ((+) <$> mul2 <*> add10) 3
-- print $ (liftA2 (+) mul2 add10) 3

-- putStrLn "hello  world"
