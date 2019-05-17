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

-- import           Test.QuickCheck hiding (Success)
import           Text.Printf
import           GHC.Exts(IsString(..))

type TypeError = Pow String

type Interp t s x y =
  Fix (Strat,t) t
    (ConstT Context
     (ReaderT StratEnv
      (EnvironmentT t
       (ExceptT ()
        (ErrorT TypeError
         (CompletionT
          (TerminatingT
           (FixT s () ()
            (->))))))))) x y

runInterp :: forall t x y. (Show t, PreOrd t, Identifiable t) => Widening t -> Interp t _ x y -> Int -> StratEnv -> Context -> TermEnv t -> x -> Terminating (FreeCompletion (Error (Pow String) (Except () (TermEnv t,y))))
runInterp termWidening f k senv0 ctx tenv0 a =
  runFixT stackWidening (T.widening resultWidening)
   (runTerminatingT
    (runCompletionT
     (runErrorT
      (runExceptT
       (runEnvironmentT
        (runReaderT
         (runConstT ctx
           f)))))))
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

-- eval :: Int -> Int -> Strat -> StratEnv -> Context -> TermEnv -> Term -> Terminating (FreeCompletion (Error TypeError (Except () (TermEnv,Term))))
-- eval i j s = runInterp (Shared.eval' s) i j

-- Instances -----------------------------------------------------------------------------------------

typeMismatch :: (ArrowFail e c, IsString e) => c (String,String) a
typeMismatch = lmap (\(expected,actual) -> printf "expected type %s but got type %s" (show expected) (show actual)) typeError

typeError :: (ArrowFail e c, IsString e) => c String a
typeError = lmap fromString fail
