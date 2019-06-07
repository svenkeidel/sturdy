{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module SingletonSemantics where

import           Prelude hiding ((.),fail)

import qualified ConcreteSemantics as C
import           ConcreteSemantics ()
import           SharedSemantics as Shared
import           AbstractSemantics
import           SortContext (Context)
import           Syntax hiding (Fail,TermPattern(..))
import           Utils
import           ValueT

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Abstract.Join

import           Data.Abstract.FreeCompletion hiding (Top)
import qualified Data.Abstract.FreeCompletion as Free
import           Data.Abstract.Except as E
import           Data.Abstract.Error as F
import           Data.Abstract.Singleton
import           Data.Abstract.Terminating (Terminating)
import           Data.Abstract.Widening as W
import           Data.Constructor
import           Data.Order
import           Data.Profunctor
import           Data.Term
import           Data.TermEnvironment

import           GHC.Exts(IsString(..))

type Term = Singleton C.Term

eval :: Int -> Int -> Strat -> StratEnv -> Context -> TermEnv Term -> Term -> Terminating (FreeCompletion (Error TypeError (Except () (TermEnv Term,Term))))
eval i _ strat senv ctx  = runInterp (Shared.eval' strat) i W.finite senv ctx

liftConcrete :: Arrow c => ValueT C.Term c x y -> ValueT t' c x (Singleton y)
liftConcrete (ValueT f) = Single ^<< ValueT f

-- Instances -----------------------------------------------------------------------------------------
instance (ArrowChoice c, ArrowApply c, ArrowJoin c, ArrowConst Context c, ArrowFail e c, ArrowExcept () c, IsString e, LowerBounded (c () Term))
    => IsTerm Term (ValueT Term c) where
  matchCons matchSubterms = proc (c,ps,t) -> case t of
    Single (C.Cons c' ts) | c == c' && eqLength ps ts -> do
      ts' <- matchSubterms -< (ps,map Single ts)
      case allSingle ts' of
        Nothing -> returnA -< Any
        Just cts' -> returnA -< Single $ C.Cons c cts'
    Single _ -> throw -< ()
    Any -> do
      matchSubterms -< (ps, replicate (length ps) Any)
      (returnA -< Any) <⊔> (throw -< ())

  matchString = proc (s,t) -> case t of 
    Single ct -> liftConcrete matchString -< (s,ct)
    Any -> (returnA -< Any) <⊔> (throw -< ())

  matchNum = proc (i,t) -> case t of 
    Single ct -> liftConcrete matchNum -< (i,ct)
    Any -> (returnA -< Any) <⊔> (throw -< ())

  matchExplode matchCons' matchSubterms = proc t -> case t of
    Single (C.Cons (Constructor c) ts) -> do
      matchCons' -< Single $ C.StringLiteral c
      matchSubterms -< Single $ convertToList ts
      returnA -< t
    Single (C.StringLiteral _) -> do
      matchSubterms -< Single $ convertToList []
      returnA -< t
    Single (C.NumberLiteral _) -> do
      matchSubterms -< Single $ convertToList []
      returnA -< t
    Any -> do
      matchCons' -< Any
      matchSubterms -< Any
      returnA -< Any

  buildExplode = proc (t, ts) -> case (t, ts) of
    (Single ct, Single cts) -> liftConcrete buildExplode -< (ct, cts)
    _ -> returnA -< Any

  buildCons = proc (c, ts) -> case allSingle ts of
    Just cts -> liftConcrete buildCons -< (c, cts)
    Nothing -> returnA -< Any

  buildNum = liftConcrete buildNum

  buildString = liftConcrete buildString

  equal = proc (t1,t2) -> case (t1, t2) of
    (Single ct1, Single ct2) -> liftConcrete equal -< (ct1, ct2)
    _ -> (returnA -< t1) <⊔> (throw -< ())

  mapSubterms f = proc t -> case t of
    Single ct -> case ct of
      C.Cons c ts -> do
        ts' <- f -< map Single ts
        case allSingle ts' of
          Nothing -> returnA -< Any
          Just cts' -> returnA -< Single $ C.Cons c cts'
      C.StringLiteral {} -> returnA -< t
      C.NumberLiteral {} -> returnA -< t
    Any -> (returnA -< Any) <⊔> (throw -< ())


instance (Arrow c, Profunctor c) => ArrowTop Term c where
  topA = arr (const Any)

instance Complete (FreeCompletion Term) where
  Lower x ⊔ Lower y = Lower (x ⊔ y)
  _ ⊔ _ = Free.Top
