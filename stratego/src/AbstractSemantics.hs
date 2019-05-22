{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
module AbstractSemantics where

import           Prelude hiding ((.),fail)

import           SortContext (Context)
import           Syntax hiding (Fail,TermPattern(..))
import           ValueT

import           Control.Arrow.Fix
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.Abstract.Completion
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Terminating

import           Data.TermEnvironment
import           Data.Abstract.FreeCompletion hiding (Top)
import qualified Data.Abstract.FreeCompletion as Free
import           Data.Abstract.Except as E
import           Data.Abstract.Error as F
import           Data.Abstract.DiscretePowerset(Pow)
import qualified Data.Abstract.DiscretePowerset as P
import qualified Data.Abstract.WeakMap as S
import qualified Data.Abstract.StackWidening as SW
import           Data.Abstract.Terminating (Terminating)
import qualified Data.Abstract.Terminating as T
import           Data.Abstract.Widening as W
import           Data.Order
import qualified Data.Lens as L
import           Data.Identifiable

type TypeError = Pow String

type Interp t s x y =
  Fix (Strat,t) t
   (ValueT t
    (ReaderT StratEnv
     (EnvironmentT t
      (ConstT Context
       (ExceptT ()
        (ErrorT TypeError
         (CompletionT
          (TerminatingT
           (FixT s () ()
            (->)))))))))) x y

runInterp :: forall t x y. (Show t, PreOrd t, Identifiable t) => Interp t _ x y -> Int -> Widening t -> StratEnv -> Context -> TermEnv t -> x -> Terminating (FreeCompletion (Error (Pow String) (Except () (TermEnv t,y))))
runInterp f k termWidening senv0 ctx tenv0 a =
  runFixT stackWidening (T.widening resultWidening)
   (runTerminatingT
    (runCompletionT
     (runErrorT
      (runExceptT
       (runConstT ctx
        (runEnvironmentT
         (runReaderT
          (runValueT f))))))))
    (tenv0, (senv0, a))
  where
    stackWidening :: SW.StackWidening _ (TermEnv t, (StratEnv, (Strat, t)))
    stackWidening = SW.filter' (L.second (L.second (L.first stratCall)))
                  $ SW.groupBy (L.iso' (\(tenv,(senv,(strat,term))) -> ((strat,senv),(term,tenv)))
                                       (\((strat,senv),(term,tenv)) -> (tenv,(senv,(strat,term)))))
                  $ SW.stack
                  $ SW.reuseFirst
                  $ SW.maxSize k
                  $ error "top"

    resultWidening :: Widening (FreeCompletion (Error TypeError (Except () (TermEnv t,t))))
    resultWidening = Free.widening (F.widening P.widening (E.widening (\_ _ -> (Stable,())) (S.widening termWidening W.** termWidening)))

