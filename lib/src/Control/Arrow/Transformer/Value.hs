{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Arrow Transformer that can be used to define value operations:
-- 
-- > instance IsVal Val (ValueT Val c) where ...
-- 
-- ATTENTION: 'ArrowComplete' should be defined on a case by case basis.
-- For example, we could define a 'ArrowComplete' instance for values
-- that turns @Top@ into an error:
-- 
-- > instance ArrowComplete Val c => ArrowComplete Val (ValueT Val c) where
-- >   ValueT f <⊔> ValueT g = ValueT $ proc x -> do
-- >     v <- f <⊔> g -< x
-- >     case v of
-- >       Top -> fail "Encountered top while joining values."
-- >       v' -> returnA -< v'
module Control.Arrow.Transformer.Value where

import Prelude hiding ((.))

import Control.Category
import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Trans
import Control.Arrow.Order
import Control.Arrow.Environment
import Control.Arrow.Store
import Control.Arrow.Except

import Data.Profunctor
import Data.Coerce

newtype ValueT val c x y = ValueT {runValueT :: c x y}
  deriving (Profunctor,Category,Arrow,ArrowChoice,
            ArrowEnv var val',ArrowClosure var val' env,ArrowStore addr val',
            ArrowExcept exc,ArrowFail e,
            ArrowLowerBounded)

instance ArrowRun c => ArrowRun (ValueT val c) where
  type Rep (ValueT val c) x y = Rep c x y
  run = run . runValueT

instance ArrowTrans (ValueT val) where
  type Dom (ValueT val) x y = x
  type Cod (ValueT val) x y = y
  lift = coerce
  unlift = coerce
  {-# INLINE lift #-}
  {-# INLINE unlift #-}

type instance Fix x y (ValueT val c) = ValueT val (Fix x y c)
deriving instance ArrowFix x y c => ArrowFix x y (ValueT val c)

