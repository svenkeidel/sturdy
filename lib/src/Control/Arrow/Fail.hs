{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Fail where

import Prelude hiding (fail)

import           Control.Arrow
import           Control.Monad.Except (MonadError)
import qualified Control.Monad.Except as M

-- | Arrow-based interface for computations that can fail.
class Arrow c => ArrowFail e c | c -> e where

  -- | Causes the computation to fail. In contrast to
  -- 'Control.Arrow.Except.ArrowExcept', this failure cannot be recovered from.
  fail :: c e x

instance MonadError e m => ArrowFail e (Kleisli m) where
  fail = Kleisli M.throwError

-- | Simpler version of 'fail'.
fail' :: ArrowFail () c => c a b
fail' = arr (const ()) >>> fail
