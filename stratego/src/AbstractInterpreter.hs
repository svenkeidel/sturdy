{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module AbstractInterpreter where

import           Prelude hiding ((.),fail)

import           SortContext (Context)
import           Syntax hiding (Fail,TermPattern(..))
import           Abstract.TermEnvironment

import           Control.Category
import           Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Context as Context
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.Abstract.Completion
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Chaotic
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Stack
import           Control.Arrow.Transformer.Abstract.Fix.Cache
import           Control.Arrow.Transformer.Abstract.Fix.Trace

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
import           Data.Abstract.CallString(CallString)
import           Data.Order
import           Data.Identifiable
import           Data.Label

import           Text.Printf

type TypeError = Pow String

type Dom t = ((Strat,StratEnv),(t,TermEnv t))
type Cod t = Terminating (FreeCompletion (Error (Pow String) (Except () (TermEnv t,t))))
type Interp t x y =
  Fix
   (ValueT t
    (ConstT Context
     (ReaderT StratEnv
      (EnvT t
       (ExceptT ()
        (ErrorT TypeError
         (CompletionT
          (TerminatingT
           (FixT () ()
            (--TraceT
             (ChaoticT (Dom t)
              (StackT Stack (Dom t)
               (CacheT (Group Cache) (Dom t) (Cod t)
                (ContextT (CallString Label) (t,TermEnv t)
                 (->)))))))))))))))
   (Strat,t) t x y

runInterp :: forall t x y. (Show t, Complete t, Complete (FreeCompletion t), Identifiable t, ?sensitivity :: Int) =>
  Interp t x y -> W.Widening t -> StratEnv -> Context -> TermEnv t -> x -> Terminating (FreeCompletion (Error (Pow String) (Except () (TermEnv t,y))))
runInterp f termWidening senv0 ctx tenv0 a =
  snd (run f ctx iterationStrategy resultWidening (tenv0, (senv0, a)))
  where
    iterationStrategy
      = trace (\(tenv,(_senv,(strat,term))) -> printf "STRAT %s\nTERM  %s\nENV   %s" (show strat) (show term) (show tenv))
              (printf "RET   %s" . show . fmap (fmap (fmap (fmap snd))))
              -- (\cx -> printf "CTX   %s" (show cx))
              -- (\cache -> printf "CACHE %s" (show [ (strat,ctx,a,b,s) | ((strat,_),cache') <- toList cache, (ctx,(a,_),b,s) <- toList cache']))
      . Fix.filter stratCall (recordCallsite ?sensitivity (\((strat,_),_) -> Just (label strat)))
      . Fix.filter stratApply
        ( joinByContext' (termWidening W.** S.widening termWidening)
        . iterateInner
        )

    {-# INLINE iterationStrategy #-}

    resultWidening :: W.Widening (Terminating (FreeCompletion (Error TypeError (Except () (TermEnv t,t)))))
    resultWidening = T.widening (Free.widening (F.widening P.widening (E.widening (\_ _ -> (Stable,())) (S.widening termWidening W.** termWidening))))
    {-# INLINE resultWidening #-}
{-# INLINE runInterp #-}
