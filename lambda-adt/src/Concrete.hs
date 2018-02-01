{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
module Concrete where

import           Prelude
import           Shared

import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Try
import           Control.Arrow.Utils
import           Control.Monad.Reader

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text,unpack)

import           Text.Printf

data Val = ClsV Text Expr Env | ConV Constructor [Val] deriving (Eq)
type Env = Map Text Val

type Interp = Kleisli (ReaderT Env (Either String))

evalConcrete :: Env -> Expr -> Either String Val
evalConcrete env e = runReaderT (runKleisli eval e) env

instance ArrowFix Interp where
  fixA f = f (fixA f)

instance IsVal Val Interp where
  lookup = proc x -> do
    env <- askA -< ()
    case M.lookup x env of
      Just v -> returnA -< v
      Nothing -> failA -< printf "Variable %s not bound." x

  con = arr (uncurry ConV)
  
  match ev =
    arr (\(v,ps) -> [(v,p,e) | (p,e) <- ps]) >>>
    tryFirst (proc (v,p,e) -> do
                env <- askA -< ()
                (env',()) <- runStateArrow go -< (env,(v,p))
                localA ev -< (env',e))
             (proc () -> failA -< "no pattern matched")
    where
      go :: StateArrow Env Interp (Val,Pat) ()
      go = proc (v,p) -> case (v,p) of
        (ClsV {},_) -> failA -< "Cannot match against a closures"
        (ConV x vs, ConP y ps)
          | x == y -> voidA $ zipWithA go -< (vs,ps)
          | otherwise -> failA -< "Pattern does not match"
        (_, VarP x) -> modifyA insertVar -< (x,v)

      insertVar :: Arrow c => c ((Text,Val),Env) Env
      insertVar = arr $ uncurry $ uncurry M.insert

instance IsClosure Expr Val Interp where
  closure = proc (x,body) ->do
    env <- askA -< ()
    returnA -< ClsV x body env
  applyClosure ev = proc (cls,arg) -> case cls of
    ClsV x body env -> localA ev -< (M.insert x arg env,body)
    _ -> failA -< printf "Expected a closure, but got %s" (show cls)

instance Show Val where
  show (ClsV x e _) = "λ" ++ unpack x ++ ". " ++ show e
  show (ConV c ts) = show c ++ show ts
