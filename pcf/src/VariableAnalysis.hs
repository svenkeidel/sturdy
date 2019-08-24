{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-type-signatures #-}
module VariableAnalysis where

import           Prelude hiding (Bounded,fail,(.),exp,filter)

import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Fix as Fix
import           Control.Arrow.Trans
import           Control.Arrow.Environment as Env
import           Control.Arrow.Order hiding (bottom)
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.FreeVars
import           Control.Arrow.Transformer.Abstract.Environment(EnvT)
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Finite

import           Data.Empty
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.HashSet(HashSet)
import           Data.Text (Text)

import qualified Data.Abstract.StrongMap as SM
import           Data.Abstract.DiscretePowerset (Pow)
import           Data.Maybe
    
import           GHC.Exts(IsString(..))

import           Syntax (Expr(..),apply)
import           GenericInterpreter as Generic

type Val = ()

variables :: Expr -> HashMap Expr (HashSet Text,HashSet Text)
variables e =
  M.fromList
  $ M.foldlWithKey' (\l (scope,(expr,_)) (used,_) -> (expr,(fromMaybe (error "top") (SM.keys scope),used)):l) []
  $ fst
  $ run (Generic.eval ::
        Fix Expr Val
         (ValueT Val
           (ErrorT (Pow String)
             (FreeVarsT Text
               (EnvT Text Val
                 (FixT _ _
                   (FiniteT _ _ 
                     (->))))))) Expr Val)
    iterationStrategy
    (empty,e)
  where
    iterationStrategy = Fix.filter apply $ finite

instance (IsString e, ArrowChoice c, ArrowFail e c, ArrowComplete Val c) => IsNum Val (ValueT Val c) where
  type Join y (ValueT Val c) = ArrowComplete y (ValueT Val c)
  succ = proc _ -> returnA -< ()
  pred = proc _ -> returnA -< ()
  zero = proc _ -> returnA -< ()
  if_ f g = proc ((),(x,y)) -> (f -< x) <⊔> (g -< y)

instance (IsString e, ArrowChoice c, ArrowFail e c, ArrowEnv Text () c) => IsClosure Val (ValueT Val c) where
  closure f = proc e0 -> case e0 of
    Lam x e _ -> Env.extend f -< (x,(),e)
    Y e _ -> f -< e
    _ -> fail -< "unexpected expression in closure"
  applyClosure _ = ValueT $ proc _ -> returnA -< ()

deriving instance ArrowComplete Val c => ArrowComplete Val (ValueT Val c)
