{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Trans where

import Control.Arrow
import Data.Profunctor
import Data.Coerce

class (Arrow c, Profunctor c) => ArrowRun c where
  type Run c x y
  run :: c x y -> Run c x y

  default run :: (Underlying c x y ~ c' x' y', Run c x y ~ Run c' x' y', ArrowRun c', ArrowTrans c) => c x y -> Run c x y
  run = run . unlift
  {-# INLINE run #-}

instance ArrowRun (->) where
  type Run (->) x y = x -> y
  run = id
  {-# INLINE run #-}

class ArrowLift t where
  lift' :: (Arrow c, Profunctor c) => c x y -> t c x y

-- | Lifts an inner computation into an arrow transformer and vice versa.
class ArrowTrans c where
  type Underlying c x y :: *
  lift :: Underlying c x y -> c x y
  unlift :: c x y -> Underlying c x y

  default lift :: forall x y. (Coercible (c x y) (Underlying c x y)) => Underlying c x y -> c x y
  lift = coerce
  {-# INLINE lift #-}

  default unlift :: forall x y. (Coercible (c x y) (Underlying c x y)) => c x y -> Underlying c x y
  unlift = coerce
  {-# INLINE unlift #-}

lift1 :: ArrowTrans c => (Underlying c x y -> Underlying c x' y') -> (c x y -> c x' y')
lift1 f = lift . f . unlift
{-# INLINE lift1 #-}

unlift1 :: ArrowTrans c => (c x y -> c x' y') -> (Underlying c x y -> Underlying c x' y')
unlift1 f = unlift . f . lift
{-# INLINE unlift1 #-}
