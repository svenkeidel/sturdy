{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wno-orphans #-}
module SingletonSemantics where

import           Prelude hiding ((.),fail)

import qualified ConcreteInterpreter as C
--import           ConcreteInterpreter ()
import           GenericInterpreter as Generic
import           AbstractInterpreter
import           SortContext (Context)
import           Syntax hiding (Fail,TermPattern(..))
import           Abstract.TermEnvironment
import           Utils

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Order
import           Control.Arrow.Transformer.Value

import           Data.Abstract.FreeCompletion hiding (Top)
import qualified Data.Abstract.FreeCompletion as Free
import           Data.Abstract.Except as E
import           Data.Abstract.Error as F
import           Data.Abstract.Singleton
import           Data.Abstract.Terminating (Terminating)
import           Data.Abstract.Widening as W
import           Data.Constructor
import           Data.Order
import           Data.Term
import           Data.Profunctor

type Term = Singleton C.Term

eval :: (?sensitivity :: Int) => Int -> Strat -> StratEnv -> Context -> TermEnv Term -> Term -> Terminating (FreeCompletion (Error TypeError (Except () (TermEnv Term,Term))))
eval _ strat senv ctx  = runInterp (Generic.eval strat) W.finite senv ctx

-- Instances -----------------------------------------------------------------------------------------
instance (ArrowChoice c, ArrowComplete Term c, ArrowConst Context c, ArrowFail e c, ArrowExcept () c, ArrowLowerBounded c)
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

  {-# INLINE matchCons #-}
  {-# INLINE matchString #-}
  {-# INLINE matchNum #-}
  {-# INLINE matchExplode #-}
  {-# INLINE buildCons #-}
  {-# INLINE buildString #-}
  {-# INLINE buildNum #-}
  {-# INLINE buildExplode #-}
  {-# INLINE equal #-}
  {-# INLINE mapSubterms #-}

-- instance (Arrow c, Profunctor c) => ArrowTop Term c where
--   topA = arr (const Any)

instance (ArrowChoice c, ArrowComplete Term c) => ArrowComplete Term (ValueT Term c) where
  ValueT f <⊔> ValueT g = ValueT $ proc x -> f <⊔> g -< x

instance Complete (FreeCompletion Term) where
  Lower x ⊔ Lower y = Lower (x ⊔ y)
  _ ⊔ _ = Free.Top

liftConcrete :: Profunctor c => ValueT C.Term c x y -> ValueT t' c x (Singleton y)
liftConcrete (ValueT f) = rmap Single (ValueT f)
{-# INLINE liftConcrete #-}
