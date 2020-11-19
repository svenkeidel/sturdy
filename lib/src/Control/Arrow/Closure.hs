{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Closure where

import Control.Arrow hiding (ArrowMonad)
import Control.Arrow.Monad
import Control.Arrow.Trans
import Control.Arrow.Transformer.Cokleisli
import Control.Arrow.Transformer.Const
import Control.Arrow.Transformer.Kleisli
import Control.Arrow.Transformer.Reader
import Control.Arrow.Transformer.State
import Control.Arrow.Transformer.Static
import Control.Arrow.Transformer.Writer

import Data.Profunctor
import Data.Monoidal

import GHC.Exts

class (Arrow c, Profunctor c) => ArrowClosure expr cls c | cls -> expr where
  type Join y cls c :: Constraint

  -- | creates a non-recursive closure from expression.
  closure :: c expr cls

  -- | Apply a closure in its closed environment.
  apply :: Join y cls c => c (expr,x) y -> c (cls, x) y

  -- default lifting
  default closure :: (c ~ t c', ArrowTrans t, ArrowClosure expr cls c') => c expr cls
  closure = lift' closure
  {-# INLINE closure #-}

class IsClosure cls env where
  mapEnvironment :: (env -> env) -> cls -> cls
  traverseEnvironment :: Applicative f => (env -> f env) -> cls -> f cls
  setEnvironment :: env -> cls -> cls
  setEnvironment env = mapEnvironment (const env)
  {-# INLINE setEnvironment #-}


------------- Instances --------------
instance (ArrowComonad f c, ArrowClosure expr cls c) => ArrowClosure expr cls (CokleisliT f c) where
  type Join y cls (CokleisliT f c) = Join y cls c
  apply f = lift (lmap costrength2 (apply (lmap strength2 (unlift f))))
  {-# INLINE apply #-}

instance ArrowClosure expr cls c => ArrowClosure expr cls (ConstT r c) where
  type Join y cls (ConstT r c) = Join y cls c
  apply f = lift $ \r -> apply (unlift f r)
  {-# INLINE apply #-}

instance (ArrowMonad f c, ArrowClosure expr cls c) => ArrowClosure expr cls (KleisliT f c) where
  type Join y cls (KleisliT f c) = Join (f y) cls c
  apply f = lift (apply (unlift f))
  {-# INLINE apply #-}

instance ArrowClosure expr cls c => ArrowClosure expr cls (ReaderT r c) where
  type Join y cls (ReaderT r c) = Join y cls c
  apply f = lift $ lmap shuffle1 $ apply (lmap shuffle1 (unlift f))
  {-# INLINE apply #-}

instance ArrowClosure expr cls c => ArrowClosure expr cls (StateT s c) where
  type Join y cls (StateT s c) = Join (s,y) cls c
  apply f = lift $ lmap shuffle1 (apply (lmap shuffle1 (unlift f)))
  {-# INLINE apply #-}

instance (Applicative f, ArrowClosure expr cls c) => ArrowClosure expr cls (StaticT f c) where
  type Join y cls (StaticT f c) = Join y cls c
  apply (StaticT f) = StaticT $ apply <$> f
  {-# INLINE apply #-}
  {-# SPECIALIZE instance ArrowClosure expr cls c => ArrowClosure expr cls (StaticT ((->) r) c) #-}

instance (Monoid w, ArrowClosure expr cls c) => ArrowClosure expr cls (WriterT w c) where
  type Join y cls (WriterT w c) = Join (w,y) cls c
  apply f = lift (apply (unlift f))
  {-# INLINE apply #-}
