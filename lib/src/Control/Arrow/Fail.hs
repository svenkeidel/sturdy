{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Fail where

import Prelude hiding (fail)

import           Control.Arrow
import           Data.Profunctor

import GHC.Exts (Constraint)

-- | Arrow-based interface for computations that can fail.
class (Arrow c, Profunctor c) => ArrowFail e c | c -> e where
  type Join y c :: Constraint

  -- | Causes the computation to fail. In contrast to
  -- 'Control.Arrow.Except.ArrowExcept', this failure cannot be recovered from.
  fail :: Join y c => c e y

-- | Simpler version of 'fail'.
fail' :: (Join b c, ArrowFail () c) => c a b
fail' = arr (const ()) >>> fail
