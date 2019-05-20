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
-- import qualified Prelude as P

import           SharedSemantics as Shared
import           Sort
import           SortContext (Context,Signature(..))
import qualified SortContext as Ctx
-- import           Soundness
import           Syntax hiding (Fail,TermPattern(..))
import           Utils

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Deduplicate
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Trans
import           Control.Arrow.Abstract.Join
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Abstract.Completion
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.DeepSeq

import           Data.TermEnvironment
import           Data.Abstract.FreeCompletion hiding (Top)
import qualified Data.Abstract.FreeCompletion as Free
import           Data.Abstract.Except as E
import           Data.Abstract.Error as F
import qualified Data.Abstract.Maybe as A
import           Data.Abstract.DiscretePowerset(Pow)
import qualified Data.Abstract.DiscretePowerset as P
-- import qualified Data.Concrete.Powerset as C
-- import qualified Data.Concrete.Error as CE
-- import qualified Data.Concrete.Failure as CF
import           Data.Abstract.WeakMap (Map)
import qualified Data.Abstract.WeakMap as S
import qualified Data.Abstract.StackWidening as SW
import           Data.Abstract.Terminating (Terminating)
import qualified Data.Abstract.Terminating as T
import           Data.Abstract.Widening as W
import           Data.Foldable (foldr')
-- import           Data.GaloisConnection
import qualified Data.HashMap.Lazy as M
import           Data.Hashable
import           Data.Order
import           Data.Profunctor
import qualified Data.Lens as L
import           Data.Identifiable
import           Data.Coerce

-- import           Test.QuickCheck hiding (Success)
import           Text.Printf
import           GHC.Exts(IsString(..))

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

type instance Fix x y (ValueT t c) = ValueT t (Fix (Dom (ValueT t) x y) (Cod (ValueT t) x y) c)
newtype ValueT t c x y = ValueT { runValueT :: c x y }
  deriving (Category,Profunctor,Arrow,ArrowChoice,ArrowJoin,ArrowFail e,ArrowExcept e,ArrowFix a b,ArrowDeduplicate a b,ArrowReader r,IsTermEnv env t,ArrowConst r)

instance ArrowTrans (ValueT t) where
  type Dom (ValueT t) x y = x
  type Cod (ValueT t) x y = y
  lift = ValueT
  unlift = runValueT

instance (Profunctor c, ArrowApply c) => ArrowApply (ValueT t c) where
  app = ValueT $ lmap (first coerce) app

deriving instance (PreOrd (c (Dom (ValueT t) x y) (Cod (ValueT t) x y))) => PreOrd (ValueT t c x y)
deriving instance (LowerBounded (c (Dom (ValueT t) x y) (Cod (ValueT t) x y))) => LowerBounded (ValueT t c x y)
deriving instance (Complete (c (Dom (ValueT t) x y) (Cod (ValueT t) x y))) => Complete (ValueT t c x y)
