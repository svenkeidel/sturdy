{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Fix.Context where

import Control.Arrow
import Control.Arrow.Trans
import Data.Profunctor

class (Arrow c, Profunctor c) => ArrowContext ctx c | c -> ctx where
  askContext :: c () ctx
  localContext :: c x y -> c (ctx,x) y

  default askContext :: (c ~ t c', ArrowLift t, ArrowContext ctx c') => c () ctx
  askContext = lift' askContext
  {-# INLINE askContext #-}

class (Arrow c, Profunctor c) => ArrowJoinContext a c | c -> a where
  joinByContext :: c a a

  default joinByContext :: (c ~ t c', ArrowLift t, ArrowJoinContext a c') => c a a
  joinByContext = lift' joinByContext
  {-# INLINE joinByContext #-}
