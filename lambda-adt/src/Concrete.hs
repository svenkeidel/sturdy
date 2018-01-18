{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
module Concrete where

import           Shared

import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Monad.Reader
import           Control.Monad.Except

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)

import           Text.Printf

data Val = ClsV Closure | ConV Text [Val] deriving (Eq,Show)
type Env = Map Text Val
data Closure = Closure Expr Env deriving (Eq,Show)

type Interp = Kleisli (ReaderT Env (Either String))

instance IsVal Val Interp where
  lookup = proc x -> do
    env <- Kleisli (const ask) -< ()
    case M.lookup x env of
      Just v -> returnA -< v
      Nothing -> failA -< printf "Variable %s not bound." x

  con = arr (uncurry ConV)
  
  match ev = proc (v,pats) -> go
    where
      go = case v of
        ClsV _ -> failA -< "Cannot perform case distinctions on closures"
        ConV x vs -> _

instance ArrowFail String Interp where
  failA = Kleisli $ \e -> throwError e
