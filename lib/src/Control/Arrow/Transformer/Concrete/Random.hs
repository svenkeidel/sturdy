{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Concrete.Random where

import           Prelude hiding ((.))

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Environment
import           Control.Arrow.Closure
import           Control.Arrow.Except
import           Control.Arrow.Trans
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.Random
import           Control.Arrow.State
import           Control.Arrow.Store

import           Control.Arrow.Transformer.State

import           Data.Profunctor
import           Data.Coerce

import           System.Random(StdGen,Random)
import qualified System.Random as R

newtype RandomT c x y = RandomT (StateT StdGen c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowLift,ArrowRun,
            ArrowConst r, ArrowReader r, ArrowFail e, ArrowExcept e,
            ArrowEnv var val, ArrowClosure expr cls, ArrowStore var val)

runRandomT :: RandomT c x y -> c (StdGen,x) (StdGen,y)
runRandomT = coerce
{-# INLINE runRandomT #-}

instance (Random v, Arrow c, Profunctor c) => ArrowRand v (RandomT c) where
  random = RandomT $ modify' (\((),gen) -> R.random gen)

type instance Fix (RandomT c) x y  = RandomT (Fix c (StdGen,x) (StdGen,y))
instance (Arrow c, ArrowFix (Underlying (RandomT c) x y)) => ArrowFix (RandomT c x y)

instance ArrowState s c => ArrowState s (RandomT c) where
  get = lift' get
  put = lift' put
