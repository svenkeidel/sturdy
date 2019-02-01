{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Abstract.Terminating(TerminatingT(..)) where

import Prelude hiding (id,(.),lookup,fail)

import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Trans
import Control.Arrow.State
import Control.Arrow.Reader
import Control.Arrow.Abstract.Terminating
import Control.Category

import Data.Abstract.Terminating
import Data.Order
import Data.Monoidal
import Data.Profunctor

-- | Arrow that propagates non-terminating computations.
newtype TerminatingT c x y = TerminatingT { runTerminatingT :: c x (Terminating y) }

instance (ArrowChoice c,Profunctor c) => ArrowTerminating (TerminatingT c) where
  throwTerminating = lift id
  catchTerminating f = lift $ rmap Terminating (unlift f) 

instance ArrowTrans TerminatingT where
  type Dom TerminatingT x y = x
  type Cod TerminatingT x y = (Terminating y)
  lift = TerminatingT
  unlift = runTerminatingT

instance ArrowLift TerminatingT where
  lift' f = lift $ rmap Terminating f

instance (Profunctor c, Arrow c) => Profunctor (TerminatingT c) where
  dimap f g h = lift $ dimap f (fmap g) (unlift h)
  lmap f h = lift $ lmap f (unlift h)
  rmap g h = lift $ rmap (fmap g) (unlift h)

instance (ArrowChoice c, Profunctor c) => Category (TerminatingT c) where
  id = lift' id
  f . g = lift $ rmap toEither (unlift g) >>> arr (const NonTerminating) ||| unlift f

instance (ArrowChoice c, Profunctor c) => Arrow (TerminatingT c) where
  arr f = lift' (arr f)
  first f = lift $ rmap strength1 (first (unlift f))
  second f = lift $ rmap strength2 (second (unlift f))
  f &&& g = lift $ rmap mstrength (unlift f &&& unlift g)
  f *** g = lift $ rmap mstrength (unlift f *** unlift g)

instance (ArrowChoice c, Profunctor c) => ArrowChoice (TerminatingT c) where
  left f = lift $ rmap strength1 $ left (unlift f)
  right f = lift $ rmap strength2 $ right (unlift f)
  f ||| g = lift $ unlift f ||| unlift g
  f +++ g = lift $ rmap mstrength (unlift f +++ unlift g)

instance (ArrowChoice c, Profunctor c, ArrowApply c) => ArrowApply (TerminatingT c) where
  app = lift $ lmap (first unlift) app

instance (ArrowChoice c, ArrowState s c) => ArrowState s (TerminatingT c) where
  get = lift' get
  put = lift' put

instance (ArrowChoice c, ArrowReader r c) => ArrowReader r (TerminatingT c) where
  ask = lift' ask
  local f = lift (local (unlift f))

instance (ArrowChoice c, ArrowConst x c) => ArrowConst x (TerminatingT c) where
  askConst = lift' askConst

deriving instance PreOrd (c x (Terminating y)) => PreOrd (TerminatingT c x y)
deriving instance LowerBounded (c x (Terminating y)) => LowerBounded (TerminatingT c x y)
deriving instance Complete (c x (Terminating y)) => Complete (TerminatingT c x y)
deriving instance CoComplete (c x (Terminating y)) => CoComplete (TerminatingT c x y)
deriving instance UpperBounded (c x (Terminating y)) => UpperBounded (TerminatingT c x y)

