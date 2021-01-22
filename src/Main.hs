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

import Control.Monad.State.Strict (MonadState (get, put), State, execState, state)
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.QuickCheck.Checkers (EqProp (..), quickBatch)
import Test.QuickCheck.Classes (applicative, functor, monad)
import Prelude

main :: IO ()
main = do
  let accumulate :: Int -> State Int ()
      accumulate x = state (\a -> ((), a + x))
  print $ execState (accumulate 2) 3
  print $ execState (traverse accumulate [1 .. 10]) 100
  -- quickCheck prop_PlainOldSameAsStateful
  let myStateful :: (StatefulComputation ()) (Int, Float, String)
      myStateful = undefined
  quickBatch $ functor myStateful
  quickBatch $ applicative myStateful
  quickBatch $ monad myStateful

data Parity = Even | Odd
  deriving (Show)

-- 1. create a plain old function that: Add `x` to `accum`. Return `x`'s Parity
--    and the new `accum`.
--    x, accum :: Int
--
-- 2. call this function for a few different `x` and some initial `accum`,
--    passing the `accum` between calls. Print the final `accum`, print the
--    final Parity.
-- 3. Translate the above to a State-ful function. I.e. the State type/State
--    monad.
-- 4. As you did in 2, but use the State monad functions (i.e. the functions in
--    this section:
--    https://www.stackage.org/haddock/lts-16.25/mtl-2.2.2/Control-Monad-State-Strict.html#g:2).
--    Including: Print the final `accum`, print the final Parity.

-- 5. use Quickcheck to write a property that:
-- for all x and for all accum, your plain old function and your State-ful function
-- compute the same accum.
prop_PlainOldSameAsStateful :: Int -> Int -> Bool
prop_PlainOldSameAsStateful = undefined

-- 6. add a whole list of integers to the accumulator, and return the new
--    accumulator. Remember (i.e. use) previous lessons/chapters :)
-- 7. write your own State type (let's call it MyState or something besides
--    State to avoid name clashes). Implement Functor, Applicative, Monad. Use
--    it instead of the imported State in the exercises above (i.e. above
--    replace usage of State with MyState). In English: the State type
--    represents a function that uses current state to produce a new value and
--    the next state.
--
-- Hint for your own MyState, see:
-- runState ::
-- State s a	-- state-passing computation to execute
-- -> s	-- initial state
-- -> (a, s)	-- return value and final state

data StatefulComputation state result = StatefulComputation (state -> (result, state))

-- Not of great interest to you; necessary for the property testing
instance (Arbitrary a) => Arbitrary ((StatefulComputation ()) a) where
  arbitrary = do
    rv <- arbitrary
    pure (StatefulComputation (\s -> (rv, s)))

-- Not of great interest to you; necessary for the property testing
instance (EqProp a) => EqProp ((StatefulComputation ()) a) where
  (StatefulComputation fl) =-= (StatefulComputation fr) =
    fl () =-= fr ()

-- Not of great interest to you; necessary for the property testing
instance (Show result) => Show (StatefulComputation () result) where
  show (StatefulComputation f) =
    let (r, _) = f ()
     in show r

instance Functor (StatefulComputation state) where
  fmap :: (a -> b) -> (StatefulComputation state) a -> (StatefulComputation state) b
  fmap = undefined
