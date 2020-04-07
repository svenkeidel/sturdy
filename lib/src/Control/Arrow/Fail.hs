{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Fail where

import Prelude hiding (fail)

import           Control.Arrow
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
