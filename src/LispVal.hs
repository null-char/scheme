{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module LispVal
  ( LispVal (..),
  )
where

import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Typeable (Typeable)

type EnvCtx = M.Map T.Text LispVal

-- Takes a list of `LispVal`s as arguments of the function
data IFunc = IFunc {fn :: [LispVal] -> Eval LispVal}

instance Eq IFunc where
  (==) _ _ = False

data LispVal
  = Atom T.Text
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bool Bool
  deriving (Typeable, Eq)

newtype Eval a = Eval {unEval :: ReaderT EnvCtx IO a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadIO,
      MonadReader EnvCtx
    )

showVal :: LispVal -> T.Text
showVal v =
  case v of
    (Atom atom) -> atom
    (List values) -> T.concat ["(", T.unwords $ showVal <$> values, ")"]
    (Number n) -> T.pack $ show n
    (String s) -> T.concat ["\"", s, "\""]
    (Fun _) -> "(internal func)"
    (Lambda _ _) -> "(lambda func)"
    Nil -> "Nil"
    (Bool True) -> "#t"
    (Bool False) -> "#f"

instance Show LispVal where
  show = T.unpack . showVal