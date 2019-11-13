{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Control.Arrow.Cont
import Control.Arrow.Const
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Trans
import Control.Arrow.Order
import Control.Arrow.Environment
import Control.Arrow.Store
import Control.Arrow.Except
import Control.Arrow.Reader
import Control.Arrow.State

import Data.Profunctor.Unsafe
import Data.Coerce

newtype ValueT val c x y = ValueT { runValueT :: c x y }
  deriving (Profunctor,Category,Arrow,ArrowChoice, ArrowConst r,
            ArrowEnv var val', ArrowLetRec var val', ArrowStore addr val',
            ArrowExcept exc,ArrowFail e,
            ArrowLowerBounded, ArrowReader r, ArrowState s, ArrowCont)

instance (ArrowApply c, Profunctor c) => ArrowApply (ValueT val c) where
  app = lift (app .# first coerce)
  {-# INLINE app #-}

instance ArrowRun c => ArrowRun (ValueT val c) where type Run (ValueT val c) x y = Run c x y
instance ArrowTrans (ValueT val c) where type Underlying (ValueT val c) x y = c x y
type instance Fix (ValueT val c) x y  = ValueT val (Fix c x y)
instance ArrowFix (Underlying (ValueT val c) x y) => ArrowFix (ValueT val c x y)

