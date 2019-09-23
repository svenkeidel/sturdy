{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Fix.Context where

import Prelude hiding ((.))

import Control.Category
import Control.Arrow
import Control.Arrow.Fix

import Data.Profunctor

class (Arrow c, Profunctor c) => ArrowContext ctx a c | c -> ctx, c -> a where
  type Widening c a :: *
  askContext :: c () ctx
  localContext :: c x y -> c (ctx,x) y
  joinByContext :: Widening c a -> c a a

joinByContext' :: ArrowContext ctx a c => Widening c a -> IterationStrategy c a b
joinByContext' widen f = f . joinByContext widen
{-# INLINE joinByContext' #-}
