{-# LANGUAGE Arrows #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Stack where

import           Control.Arrow
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State

import           Data.Profunctor.Unsafe

import Numeric.Natural (Natural)

class ArrowStack v c | c -> v where
  push :: c v ()
  pop :: c () v
  peek :: c () v
  ifEmpty :: c x y -> c x y -> c x y
  localFreshStack :: c x y -> c x y

pop2 :: (ArrowStack v c, ArrowChoice c) => c () (v, v)
pop2 = proc _ -> do
  v2 <- pop -< ()
  v1 <- pop -< ()
  returnA -< (v1, v2)

popn :: (ArrowStack v c, ArrowChoice c) => c Natural [v]
popn = proc n -> case n of
  0 -> returnA -< []
  _ -> do
    v <- pop -< ()
    vs <- popn -< n-1
    returnA -< v:vs

pushn :: (ArrowStack v c, ArrowChoice c) => c [v] ()
pushn = proc vs -> case vs of
  [] -> returnA -< ()
  v:vs' -> do
    pushn -< vs'
    push -< v


---------------- instances -------------------------

instance (Profunctor c, Arrow c, Monad f, ArrowStack v c) => ArrowStack v (KleisliT f c) where
    push = lift' push
    pop = lift' pop
    peek = lift' peek
    ifEmpty a1 a2 = lift (ifEmpty (unlift a1) (unlift a2))
    localFreshStack a = lift (localFreshStack (unlift a))

instance (Profunctor c, Arrow c, ArrowStack v c) => ArrowStack v (ReaderT r c) where
    push = lift' push
    pop = lift' pop
    peek = lift' peek
    ifEmpty a1 a2 = lift (ifEmpty (unlift a1) (unlift a2))
    localFreshStack a = lift (localFreshStack (unlift a))


instance (Profunctor c, Arrow c, ArrowStack v c) => ArrowStack v (StateT st c) where
    push = lift' push
    pop = lift' pop
    peek = lift' peek
    ifEmpty a1 a2 = lift (ifEmpty (unlift a1) (unlift a2))
    localFreshStack a = lift (localFreshStack (unlift a))

