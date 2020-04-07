{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Control.Arrow.Transformer.Abstract.LogError(LogErrorT,runLogErrorT) where

import           Prelude hiding (id,(.),lookup,read)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Environment as Env
import           Control.Arrow.Closure as Cls
import           Control.Arrow.LetRec
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Trans
import           Control.Arrow.Reader
import           Control.Arrow.Order (ArrowComplete(..))
import           Control.Arrow.Store as Store
import           Control.Arrow.Except as Exc
import           Control.Arrow.Transformer.State
import           Control.Arrow.Fix.Context

import           Data.Profunctor
import           Data.Order
import           Data.Identifiable
import           Data.Empty
import           Data.Hashed.Lazy
import           Data.HashSet (HashSet)
import qualified Data.HashSet as Set

type Set a = Hashed (HashSet a)

-- | Describes computations that can fail.
newtype LogErrorT e c x y = LogErrorT (StateT (Set e) c x y)
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift,
            ArrowConst r, ArrowReader r,
            ArrowEnv var val, ArrowClosure expr cls, ArrowStore a b, ArrowLetRec var val,
            ArrowExcept e', ArrowContext ctx)

runLogErrorT :: (Identifiable e, Profunctor c) => LogErrorT e c x y -> c x (Set e, y)
runLogErrorT f = lmap (empty,) (unlift f)
{-# INLINE runLogErrorT #-}

instance (Identifiable e, ArrowRun c) => ArrowRun (LogErrorT e c) where
  type Run (LogErrorT e c) x y = Run c x (Set e,y)
  run = run . runLogErrorT
  {-# INLINE run #-}

instance (Identifiable e, Arrow c, Profunctor c) => ArrowFail e (LogErrorT e c) where
  type Join x (LogErrorT e c) = LowerBounded x
  fail = lift $ proc (errs,e) -> returnA -< (mapHashed (Set.insert e) errs, bottom)
  {-# INLINE fail #-}

-- instance (ArrowApply c, Profunctor c) => ArrowApply (LogErrorT e c) where
--   app = LogErrorT (app .# first coerce)
--   {-# INLINE app #-}

deriving instance ArrowComplete (Set e, y) c => ArrowComplete y (LogErrorT e c)
instance ArrowFix (Underlying (LogErrorT e c) x y) => ArrowFix (LogErrorT e c x y) where
  type Fix (LogErrorT e c x y) = Fix (Underlying (LogErrorT e c) x y)
