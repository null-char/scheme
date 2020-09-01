{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module LispVal
  (
  )
where

import Control.Monad.Reader
import qualified Data.Map as M
import Data.Text as T
import Data.Typeable (Typeable)

type EnvCtx = M.Map T.Text LispVal

data LispVal
  = Atom T.Text
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bool Bool
  deriving (Typeable)

-- Takes a list of `LispVal`s as arguments of the function
data IFunc = IFunc {fn :: [LispVal] -> Eval LispVal}

newtype Eval a = Eval {unEval :: ReaderT EnvCtx IO a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadIO,
      MonadReader EnvCtx
    )
