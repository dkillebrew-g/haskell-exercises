module LispVal where

import ClassyPrelude


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

data IFunc

data EnvCtx
