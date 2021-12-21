{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import ClassyPrelude
import Control.Monad.Except
import Control.Monad.Reader

-- Some useful links:
-- https://www.stackage.org/lts-18.10/hoogle

-- this is in the branch 'exercise/monad_transformer':

-- Returns (Just x/2) when x is evenly divisible by 2.
div2 :: Int -> Maybe Int
div2 x =
  let divved = x `div` 2
   in if divved * 2 == x
        then Just divved
        else Nothing

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

-- Either () a
-- Maybe a

-- https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Error-Class.html#t:MonadError

needsEverything :: Reader Text (Maybe Int)
needsEverything = pure (Just 2)

needsSomeText :: (MonadReader Text m) => m Int
needsSomeText = undefined

needsError :: (MonadError String m) => m Int
needsError = undefined

parseAndDiv :: (MonadError String m, MonadReader Text m) => m Int
parseAndDiv = do
  -- _ <- needsSomeText
  -- _ <- needsError
  line <- ask
  let hoist msg mx = case mx of
        Nothing -> throwError msg
        Just x -> pure x
  asInt <-
    hoist "readMay failed" $
      catchError (readMay line) (\_ -> pure 0)
  divided <- hoist "div2 failed" $ div2 asInt
  pure . mul2 $ divided

parseAndDivUseMaybe :: (MonadError () m, MonadReader Text m) => m Int
parseAndDivUseMaybe = do
  line <- ask
  let mayInt :: Maybe Int
      mayInt = readMay line
      divved = div2 =<< mayInt
      multiplied = mul2 <$> divved
  case multiplied of
    Nothing -> throwError ()
    Just x -> pure x

main :: IO ()
main = do
  line <- getLine

  let --bar :: Int
      --bar = runReader parseAndDiv line
      -- baz :: Maybe Int
      -- baz = runReader parseAndDiv line

      -- returnedInt :: Int
      -- returnedInt = runReaderT parseAndDiv line

      -- parsedMaybe :: Maybe Int
      -- parsedMaybe = runReaderT parseAndDiv line

      parsedEither :: Either String Int
      parsedEither = runReaderT parseAndDiv line

  -- parsedEitherString :: Either String Int
  -- parsedEitherString = runReaderT parseAndDiv line

  -- case parsedMaybe of
  --   Nothing -> print "couldn't parse or div"
  --   Just i -> print i

  case parsedEither of
    Left errorMessage -> print ("error " <> errorMessage)
    Right i -> print i

  -- We'll call parseAndDiv, passing it the line we got via getLine
  -- if it succeeds and returns an int, then print the int.

  pure ()
