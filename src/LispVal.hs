{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LispVal where

import ClassyPrelude
import Control.Monad.Except
import Control.Monad.Reader

-- | All kinds of Lisp values
data LispVal
  = -- | A variable, and when evaluated will return some other value from the environment
    Atom Text
  | -- | Used to represent an S-Expression, among other things.
    List [LispVal]
  | Number Integer
  | String Text
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bool Bool
  deriving (Typeable)

data IFunc = IFunc {fn :: [LispVal] -> Eval LispVal}

type EnvCtx = Map Text LispVal

newtype Eval a = Eval {unEval :: ReaderT EnvCtx IO a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader EnvCtx,
      MonadIO
    )

instance Show LispVal where
  show = unpack . showVal

showVal :: LispVal -> Text
showVal val =
  case val of
    (Atom atom) -> atom
    (String str) -> concat ["\"", str, "\""]
    (Number num) -> pack $ show num
    (Bool True) -> "#t"
    (Bool False) -> "#f"
    Nil -> "Nil"
    (List contents) -> concat ["(", unwords $ showVal <$> contents, ")"]
    (Fun _) -> "(internal function)"
    (Lambda _ _) -> "(lambda function)"
