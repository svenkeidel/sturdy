{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances#-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Abstract where

import           TreeAutomata (Grammar)
import qualified TreeAutomata as G

import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Try
import           Control.Arrow.Utils
import           Control.Monad.Reader

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)
import           Data.Error
import           Data.Order

import           Text.Printf

import           GHC.Generics (Generic)

import           Shared

data Closure = Closure Text Expr Env deriving (Eq,Show)
data Val = Bot | ClsV [Closure] | ConV Grammar | Top deriving (Eq,Show)
type Env = HashMap Text Val

type Interp = Kleisli (ReaderT Env (Error String))

evalAbs :: Env -> Expr -> Error String Val
evalAbs env e = runReaderT (runKleisli eval e) env

instance ArrowFix Expr Val Interp where
  fixA f = f (fixA f)

instance IsVal Val Interp where
  lookup = proc x -> do
    env <- askA -< ()
    case M.lookup x env of
      Just v -> returnA -< v
      Nothing -> failA -< printf "Variable %s not bound." x

  con = _

  match ev = _

instance IsClosure Expr Val Interp where
  closure = proc (x,body) -> do
    env <- askA -< ()
    returnA -< ClsV (return (Closure x body env))
  applyClosure f = proc (fun, arg) -> case fun of
    ClsV cls -> lubA (proc (Closure x body env) -> localA f -< (M.insert x arg env, body)) -<< cls
    _ -> failA -< "Expected a closure"

instance PreOrd Closure where
  Closure g1 ⊑ Closure g2 = _


instance PreOrd Val where
  Bot ⊑ _ = True
  _ ⊑ Top = True
  ClsV c1 ⊑ ClsV c2 = all (\c -> any (c ⊑) c1) c1
  ConV g1 ⊑ ConV g2 = _

instance Complete Val where
  (⊔) = _

instance LowerBounded Val where
  bottom = Bot
