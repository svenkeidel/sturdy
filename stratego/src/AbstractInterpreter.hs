{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -fsimpl-tick-factor=400 #-}
module AbstractInterpreter where

import           Prelude hiding ((.),fail,id,iterate)

import           SortContext (Context)
import           Syntax hiding (Fail,TermPattern(..),id)
import           Abstract.TermEnvironment as TEnv

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fix as Fix
import           Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Reuse (ArrowReuse(reuse))
import           Control.Arrow.Trans
import           Control.Arrow.Closure as Cls
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Const
import qualified Control.Arrow.Transformer.Abstract.Environment as SEnv
import           Control.Arrow.Transformer.Abstract.Completion
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Chaotic
import           Control.Arrow.Transformer.Abstract.Fix.Stack
import           Control.Arrow.Transformer.Abstract.Fix.Cache

import           Data.Hashable
import qualified Data.HashMap.Lazy as M
import qualified Data.Abstract.Environment.Flat as Flat
import           Data.Abstract.Closure (Closure)
import qualified Data.Abstract.Closure as Cl
import           Data.Abstract.FreeCompletion hiding (Top)
import qualified Data.Abstract.FreeCompletion as Free
import           Data.Abstract.Except as E
import           Data.Abstract.Error as F
import           Data.Abstract.DiscretePowerset(Pow)
import qualified Data.Abstract.DiscretePowerset as P
import qualified Data.Abstract.Map as S
import           Data.Abstract.Terminating (Terminating)
import qualified Data.Abstract.Terminating as T
import           Data.Abstract.Stable
import           Data.Abstract.Widening as W
import           Data.Order
import           Data.Identifiable
import           Data.Profunctor
import           Data.Monoid(First(..))

import           Control.Arrow.Fix.Cache as Cache
import           Control.Arrow.Fix.Chaotic as Iterate
import           Control.Arrow.Fix.Context as Context
import           Control.Arrow.Transformer.Abstract.Fix.Trace
import           Text.Printf
import           GHC.Exts
import qualified Debug.Trace as Debug

type TypeError = Pow String
type SEnv = Flat.Env StratVar (Closure Strategy)

type Dom t = ((Hashed Strat,Hashed SEnv),(Hashed t,Hashed (TermEnv t)))
type Cod t = Terminating (FreeCompletion (Error (Pow String) (Except () (TermEnv t,t))))
type Interp t x y =
  Fix
   (ValueT t
    (ConstT Context
     (SEnv.EnvT Flat.Env StratVar (Closure Strategy)
      (TEnv.EnvT t
       (--NoInlineT
        (ExceptT ()
         (ErrorT TypeError
          (CompletionT
           (TerminatingT
            (FixT () ()
             (-- TraceT
              (ChaoticT (Dom t)
               (StackT Stack (Dom t)
                (CacheT (Group Cache) (Dom t) (Cod t)
                 (->)))))))))))))))
   (Strat,t) t x y

runInterp :: forall t x y. (Show t, Complete t, Identifiable t, ?sensitivity :: Int) =>
  Interp t x y -> W.Widening t -> StratEnv -> Context -> TermEnv t -> x -> Terminating (FreeCompletion (Error (Pow String) (Except () (TermEnv t,y))))
runInterp f termWidening senv0 ctx0 tenv0 a0 =
  snd (run f ctx0 iterationStrategy resultWidening (tenv0, (senvInit, a0)))
  where
    iterationStrategy
      =
        trace (\(tenv,(senv,(strat,term))) -> printf "STRAT %s\nTERM  %s\nENV   %s\nSENV  %s\n" (show strat) (show term) (show tenv) (show senv))
              (printf "RET   %s" . show) .
        Fix.filter stratApply
        (
          traceCache (\cache -> printf "CACHE %s" (show [ (strat,a,tenv,b,s{-senv-}) | ((strat,senv),cache') <- toList cache, ((a,tenv),b,s) <- toList cache'])) .
          -- trace' (\((strat,_senv),(term,tenv)) -> printf "STRAT %s\nTERM  %s\nENV   %s\nSENV  %s\n" (show strat) (show term) (show tenv) "" {-(show senv)-})
          --       (printf "RET   %s" . show)
          --       reuseFirst .
          reuseFirst .
          -- iterateInner
          iterateOuter
        )
    {-# INLINE iterationStrategy #-}

    senvInit = Flat.fromList [ (var,Cl.closure strat ())| (var,strat) <- M.toList senv0]

    resultWidening :: W.Widening (Terminating (FreeCompletion (Error TypeError (Except () (TermEnv t,t)))))
    resultWidening = T.widening (Free.widening (F.widening P.widening (E.widening (\_ _ -> (Stable,())) (S.widening termWidening W.** termWidening))))
    {-# INLINE resultWidening #-}
{-# INLINE runInterp #-}

reuseFirst :: (PreOrd t, ArrowChoice c, ArrowIterate (Dom t) c, ArrowReuse (Dom t) (Cod t) c) => IterationStrategy c (Dom t) (Cod t)
reuseFirst f = proc a@(_,(_,tenv)) -> do
  m <- reuse Stable (\a a' s' b' m -> case m of
                 First (Just _) -> m
                 First Nothing
                   | a ⊑ a' -> First (Just (a',b',s'))
                   | otherwise -> m) -< a
  case getFirst m of
    Just (_,b',Stable) -> returnA -< fmap (fmap (fmap (fmap (\(tenv',t') -> (tenv' `union` unhashed tenv,t'))))) b'
    _ -> f -< a
{-# INLINE reuseFirst #-}

    -- Just (a',b',Unstable) -> iterate -< (a',fmap (fmap (fmap (fmap (\(tenv',t') -> (tenv' `union` unhashed tenv,t'))))) b')
-- instance (Show t, ArrowIterate (Dom t) c) => ArrowIterate (Dom t) (TraceT c) where
--   iterate = TraceT $ proc (a@((s,_),t),b) ->
--     Iterate.iterate -< Debug.trace (printf "ITERATE\n\tx: %s\n\n" (show (s,t))) (a,b)

-- instance (Show t, ArrowCache (Dom t) (Cod t) c) => ArrowCache (Dom t) (Cod t) (TraceT c) where
--   lookup = TraceT $ proc a -> do
--     b  <- Cache.lookup -< a
--     returnA -< b -- Debug.trace (printf "LOOKUP\n\tx: %s\n\ty: %s\n\n" (show a) (show b)) b
--   update = TraceT $ proc (a@((strat,_),t),b) -> do
--     bOld  <- Cache.lookup -< a
--     (s,b') <- Cache.update -< (a,b)
--     returnA -< Debug.trace (printf "UPDATE\n\tx: %s\n\ty: %s -> %s, %s\n\n" (show (strat,t)) (show bOld) (show b') (show s))  (s,b')
--   write = TraceT $ proc (a,b,s) -> do
--     Cache.write -< (a,b,s) -- Debug.trace (printf "WRITE\n\tx: %s\n\ty: %s\n\t%s\n\t\n\n" (show a) (show b) (show s)) (a,b,s)
--   setStable = TraceT $ proc (s,a@((strat,_),t)) ->
--     Cache.setStable -< Debug.trace (printf "STABLE\n\tx: %s\n\t%s\n\n" (show (strat,t)) (show s)) (s,a)
--   {-# INLINE lookup #-}
--   {-# INLINE update #-}
--   {-# INLINE write #-}
--   {-# INLINE setStable #-}

instance ArrowClosure Strategy cls c => ArrowClosure Strategy cls (ValueT val c) where
  type Join y (ValueT val c) = Cls.Join y c
  closure = ValueT Cls.closure
  apply (ValueT f) = ValueT $ Cls.apply f
  {-# INLINE closure #-}
  {-# INLINE apply #-}
