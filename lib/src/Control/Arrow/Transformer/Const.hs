{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Control.Arrow.Transformer.Const where

import Prelude hiding (id,(.),lookup,read)

import Control.Category

import Control.Arrow
import Control.Arrow.Deduplicate
import Control.Arrow.Environment
import Control.Arrow.Fail
import Control.Arrow.Except
import Control.Arrow.Fix
import Control.Arrow.Trans
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store
import Control.Arrow.Const
import Control.Arrow.Writer
import Control.Arrow.Transformer.Static
import Control.Arrow.Abstract.Join

import Data.Order

-- | Passes along constant data.
newtype ConstT r c x y = ConstT (StaticT ((->) r) c x y)

runConstT :: r -> ConstT r c x y -> c x y
runConstT r (ConstT (StaticT f)) = f r

instance ArrowFix x y c => ArrowFix x y (ConstT r c) where
  fix f = ConstT $ StaticT $ \r -> fix (runConstT r . f . lift')

instance Arrow c => ArrowConst r (ConstT r c) where
  askConst = ConstT $ StaticT $ \r -> arr (const r)

instance ArrowApply c => ArrowApply (ConstT r c) where
  app = ConstT $ StaticT $ \r -> (\(ConstT (StaticT f),x) -> (f r,x)) ^>> app

deriving instance ArrowJoin c => ArrowJoin (ConstT r c)
deriving instance ArrowLift (ConstT r)
deriving instance Arrow c => Category (ConstT r c)
deriving instance Arrow c => Arrow (ConstT r c)
deriving instance ArrowChoice c => ArrowChoice (ConstT r c)
deriving instance ArrowLoop c => ArrowLoop (ConstT r c)
deriving instance ArrowState s c => ArrowState s (ConstT r c)
deriving instance ArrowReader r c => ArrowReader r (ConstT r' c)
deriving instance ArrowWriter w c => ArrowWriter w (ConstT r c)
deriving instance ArrowEnv var val env c => ArrowEnv var val env (ConstT r c)
deriving instance ArrowStore var val c => ArrowStore var val (ConstT r c)
deriving instance ArrowFail e c => ArrowFail e (ConstT r c)
deriving instance ArrowExcept e c => ArrowExcept e (ConstT r c)
deriving instance ArrowDeduplicate x y c => ArrowDeduplicate x y (ConstT r c)

deriving instance PreOrd (c x y) => PreOrd (ConstT r c x y)
deriving instance Complete (c x y) => Complete (ConstT r c x y)
deriving instance CoComplete (c x y) => CoComplete (ConstT r c x y)
deriving instance UpperBounded (c x y) => UpperBounded (ConstT r c x y)
deriving instance LowerBounded (c x y) => LowerBounded (ConstT r c x y)
