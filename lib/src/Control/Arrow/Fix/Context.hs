{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Fix.Context where

import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.State

import Data.Profunctor

class (Arrow c, Profunctor c) => ArrowContext ctx c | c -> ctx where
  askContext :: c () ctx
  localContext :: c x y -> c (ctx,x) y

class ArrowJoinContext cache a b c where
  type Widening cache a :: *
  joinContexts' :: Widening cache a -> IterationStrategy c (cache a b, a) b

joinContexts :: forall a cache b c. (ArrowState (cache a b) c, ArrowJoinContext cache a b c) => Widening cache a -> IterationStrategy c a b
joinContexts widen f = proc a -> do
  cache <- get -< ()
  joinContexts' widen (proc (cache,a) -> do
    put -< cache
    f -< a) -< (cache,a)
{-# INLINE joinContexts #-}
