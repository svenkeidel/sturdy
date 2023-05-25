{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wno-orphans -fsimpl-tick-factor=400 #-}
module AbstractInterpreter where

import           Prelude as Prelude hiding ((.),fail,id,iterate)

import           SortContext (Context)
import           Syntax hiding (Fail,TermPattern(..),id)
import           Abstract.TermEnvironment as TEnv

import           Control.Category
import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Context (callsiteSensitive)
import           Control.Arrow.Fix.Reuse(ArrowReuse(..))
import           Control.Arrow.Fix.Stack(reuseFirst)
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Const
import qualified Control.Arrow.Transformer.Abstract.Environment as SEnv
import           Control.Arrow.Transformer.Abstract.Completion
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Fix.Chaotic
import           Control.Arrow.Transformer.Abstract.Fix.Stack
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable
import           Control.Arrow.Transformer.Abstract.Fix.Component as Comp
import           Control.Arrow.Transformer.Abstract.Fix.CallSite(CallSiteT)
import           Control.Arrow.Transformer.Abstract.Fix.Context (ContextT) 
import qualified Control.Arrow.Transformer.Abstract.Fix.Context as Ctx

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
import           Data.Abstract.CallString as CallString
import           Data.Order
import           Data.Label
import           Data.Identifiable
import           Data.Hashable(Hashable(hash))
import           Data.Monoid(First(..))
import           GHC.Exts(IsList(..))

import           Text.Printf(printf)

import           Prettyprinter


type TypeError = Pow String
type SEnv = Flat.Env StratVar (Closure Strategy)

type Dom t = ((Strat, SEnv), (t, TermEnv t))
type Cod t = Terminating (FreeCompletion (Error (Pow String) (Except () (TermEnv t,t))))
type Interp t =
   ValueT t
    (ConstT Context
     (SEnv.EnvT SEnv
      (TEnv.EnvT t
       --NoInlineT
        (ExceptT ()
         (ErrorT TypeError
          (CompletionT
           (TerminatingT
            (FixT
             -- TraceT
              (ComponentT Comp.Component (Dom t)
               (StackT Stack (Dom t)
                (CacheT (Group Cache) (Dom t) (Cod t)
                 (->))))))))))))

runInterp :: forall t x y. (Pretty t, Show t, Complete t, Identifiable t, ?sensitivity :: Int)
          => (FixpointAlgorithm (Fix (Interp t (Strat,t) t)) -> Interp t x y)
          -> W.Widening t
          -> StratEnv
          -> Context
          -> TermEnv t
          -> x
          -> Terminating (FreeCompletion (Error (Pow String) (Except () (TermEnv t,y))))
runInterp f termWidening senv0 ctx0 tenv0 a0 =
  -- let ?contextWidening = termWidening W.** S.widening termWidening in
  let ?cacheWidening = T.widening (Free.widening (F.widening P.widening (E.widening (\_ _ -> (Stable,())) (S.widening termWidening W.** termWidening)))) in
  let algorithm =
        Fix.fixpointAlgorithm $
        -- Fix.trace' (\(tenv,(senv,(strat,term))) -> printf "STRAT %s\nTERM  %s\nENV   %s\nSENV  %s\n" (show strat) (show term) (show tenv) (show senv)) (\_ -> "") $
        Fix.filterPrism stratApply
        (
          reuseFirst .
          Fix.trace (\((strat,senv),(term,tenv)) ->
                       vsep [ "STRAT" <+> pretty strat <> "@" <> pretty (hash strat)
                            , "TERM " <+> pretty term <> "@" <> pretty (hash term)
                            , "TENV " <+> pretty tenv <> "@" <> pretty (hash tenv)
                            , "SENV " <+> pretty (hash senv)
                            ])
                     (\ret -> pretty ret <> "@" <> pretty (hash ret)) .
          Fix.traceCache (\(cache :: Group Cache (Dom t) (Cod t)) ->
                            show $ vsep [ vsep [pretty strat <+> pretty (hash strat),
                                                pretty term <+> pretty (hash term),
                                                pretty tenv <+> pretty (hash tenv),
                                                pretty (hash senv), -- pretty senv <+> pretty (hash senv),
                                                pretty ret <+> pretty (hash ret),
                                                pretty stable]
                                        | ((strat,senv),cache')    <- toList cache,
                                          ((term,tenv),ret,stable) <- toList cache']) .
            outermost
        )
  in snd (run (f algorithm) ctx0 (tenv0, (senvInit, a0)))
  where
    senvInit = Flat.fromList [ (var,Cl.closure strat ())| (var,strat) <- M.toList senv0]
{-# INLINE runInterp #-}

-- reuseFirst :: (PreOrd t, ArrowChoice c, ArrowReuse (Dom t) (Cod t) c) => FixpointCombinator c (Dom t) (Cod t)
-- reuseFirst f = proc a@(_,(_,tenv)) -> do
--   m <- reuse (\_ a' s' b' -> First (Just (a',b',s'))) -< (a,Stable)
--   case getFirst m of
--     Just (_,b',Stable) -> returnA -< fmap (fmap (fmap (fmap (\(tenv',t') -> (tenv' `union` tenv,t'))))) b'
--     Just (a',_,Unstable) -> f -< a'
--     _ -> f -< a
-- {-# INLINE reuseFirst #-}

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
