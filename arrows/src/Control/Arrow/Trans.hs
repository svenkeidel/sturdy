{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Trans where

import Control.Arrow
import Data.Profunctor
import Data.Coerce

-- | This type class allows to "run" an arrow compution as a function. For example:
--
-- > f :: StateT s (ReaderT r (WriterT w (->))) x y
-- > run f :: (r,(s,x)) -> (w,(s,y))
class (Arrow c, Profunctor c) => ArrowRun c where
  -- | Defines the underlying type of an arrow computation
  type Run c x y

  -- | Interprets an arrow transformer stack as its underlying function.
  run :: c x y -> Run c x y

  default run :: (Underlying c x y ~ c' x' y', Run c x y ~ Run c' x' y', ArrowRun c', ArrowLift c) => c x y -> Run c x y
  run = run . unlift
  {-# INLINE run #-}

instance ArrowRun (->) where
  type Run (->) x y = x -> y
  run = id
  {-# INLINE run #-}

-- | Lifts an arrow computation into an surrounding arrow transformer. For example, the 'lift'' function lifts a computation @c x y@ into a stateful computation @StateT s c x y@.
class ArrowTrans t where
  lift' :: (Arrow c, Profunctor c) => c x y -> t c x y

-- | Lifts between an arrow and its underlying type. For example, this type
-- class allows to lift between the arrow type @StateT s c x y@ and its
-- underlying type @c (s,x) (s,y)@. This type class allows to implement arrow
-- code generically without having to refer to the constructor of an arrow.
class ArrowLift c where
  -- | Defines the underlying type of an arrow computation.
  type Underlying c x y

  -- | Lifts from the underlying type to the arrow type.
  lift :: Underlying c x y -> c x y

  -- | Lifts from the arrow type to the underlying type.
  unlift :: c x y -> Underlying c x y

  default lift :: forall x y. (Coercible (c x y) (Underlying c x y)) => Underlying c x y -> c x y
  lift = coerce
  {-# INLINE lift #-}

  default unlift :: forall x y. (Coercible (c x y) (Underlying c x y)) => c x y -> Underlying c x y
  unlift = coerce
  {-# INLINE unlift #-}

lift1 :: ArrowLift c => (Underlying c x y -> Underlying c x' y') -> (c x y -> c x' y')
lift1 f = lift . f . unlift
{-# INLINE lift1 #-}

unlift1 :: ArrowLift c => (c x y -> c x' y') -> (Underlying c x y -> Underlying c x' y')
unlift1 f = unlift . f . lift
{-# INLINE unlift1 #-}
