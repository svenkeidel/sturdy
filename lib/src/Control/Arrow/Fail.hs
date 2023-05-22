{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Fail where

import Prelude hiding (fail)

import           Control.Arrow hiding (ArrowMonad)
import           Control.Arrow.Monad
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.Cokleisli
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Cont
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Static
import           Control.Arrow.Transformer.Writer

import           Data.Profunctor

import           GHC.Exts(IsString(..),Constraint)


-- | Arrow-based interface for computations that can fail.
class (Arrow c, Profunctor c) => ArrowFail e c | c -> e where
  type Join x c :: Constraint

  -- | Causes the computation to fail. In contrast to
  -- 'Control.Arrow.Except.ArrowExcept', this failure cannot be recovered from.
  fail :: Join x c => c e x

-- | Simpler version of 'fail'.
fail' :: (Join b c, ArrowFail () c) => c a b
fail' = arr (const ()) >>> fail
{-# INLINE fail' #-}

failString :: (Join x c, ArrowFail e c, IsString e) => c String x
failString = fromString ^>> fail
{-# INLINE failString #-}

------------- Instances --------------
instance (ArrowComonad f c, ArrowFail e c) => ArrowFail e (CokleisliT f c) where
  type Join y (CokleisliT f c) = Join y c
  fail = lift' fail
  {-# INLINE fail #-}

instance ArrowFail e c => ArrowFail e (ConstT r c) where
  type Join x (ConstT r c) = Join x c
  fail = lift $ \_ -> fail
  {-# INLINE fail #-}

instance (ArrowApply c, ArrowFail e c) => ArrowFail e (ContT r c) where
  type Join x (ContT r c) = Join x c
  fail = lift' fail
  {-# INLINE fail #-}

instance (ArrowMonad f c, ArrowFail e c) => ArrowFail e (KleisliT f c) where
  type Join y (KleisliT f c) = Join (f y) c
  fail = lift fail
  {-# INLINE fail #-}

instance ArrowFail e c => ArrowFail e (ReaderT r c) where
  type Join x (ReaderT r c) = Join x c
  fail = lift' fail
  {-# INLINE fail #-}

instance (ArrowFail e c) => ArrowFail e (StateT s c) where
  type Join x (StateT s c) = Join (s,x) c
  fail = lift (lmap snd fail)
  {-# INLINE fail #-}

instance (Applicative f, ArrowFail e c) => ArrowFail e (StaticT f c) where
  type Join x (StaticT f c) = Join x c
  fail = lift' fail
  {-# INLINE fail #-}
  {-# SPECIALIZE instance ArrowFail e c => ArrowFail e (StaticT ((->) r) c) #-}

instance (Monoid w, ArrowFail e c) => ArrowFail e (WriterT w c) where
  type Join x (WriterT w c) = Join x c
  fail = lift' fail
  {-# INLINE fail #-}
