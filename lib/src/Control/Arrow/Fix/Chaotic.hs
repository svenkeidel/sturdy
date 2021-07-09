{-# LANGUAGE Arrows #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Fix.Chaotic where

import           Prelude hiding (head,iterate,map)

import           Control.Arrow hiding (loop)
import           Control.Arrow.Fix
import           Control.Arrow.Fix.SCC as SCC
-- import           Control.Arrow.Fix.Metrics as Metrics
import           Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.Fix.Cache as Cache

import           Data.Abstract.Stable

innermost :: forall c a b.
             (?cacheWidening :: Widening c, ArrowChoice c,
              ArrowSCC a c, ArrowStack a c, ArrowCache a b c)
          => FixpointCombinator c a b
innermost f = proc call -> do
  resultCached <- Cache.lookup -< call
  recurrentCall <- Stack.elem -< call
  case (resultCached, recurrentCall) of
    (Just (Stable,b), _) -> returnA -< b
    (Just (Unstable,b), RecurrentCall ptr) -> do
      SCC.add -< (call, ptr)
      returnA -< b
    (Nothing, RecurrentCall ptr) -> do
      SCC.add -< (call, ptr)
      Cache.initialize -< call
    (_, NoLoop) -> do
      iterate -< call
  where
    iterate :: c a b
    iterate = proc call -> do
      resultNew <- Stack.push' f -< call
      callInSCC <- SCC.elem -< call
      case callInSCC of
        InSCC ptr -> do
          sccSize <- SCC.size -< ()
          let stable = if sccSize == 1 then Stable else Unstable
          (resultGrown, callNew, resultWidened) <- Cache.update -< (stable, call, resultNew)
          case resultGrown of
            Stable -> do
              SCC.remove -< (call, ptr)
              returnA -< resultWidened
            Unstable ->
              iterate -< callNew
        NotInSCC ->
          returnA -< resultNew
{-# INLINE innermost #-}

outermost :: forall c a b.
             (?cacheWidening :: Widening c, ArrowChoice c,
              ArrowSCC a c, ArrowStack a c, ArrowCache a b c)
          => FixpointCombinator c a b
outermost f = proc call -> do
  resultCached <- Cache.lookup -< call
  recurrentCall <- Stack.elem -< call
  case (resultCached, recurrentCall) of
    (Just (Stable,b), _) -> returnA -< b
    (Just (Unstable,b), RecurrentCall ptr) -> do
      SCC.add -< (call, ptr)
      returnA -< b
    (Nothing, RecurrentCall ptr) -> do
      SCC.add -< (call, ptr)
      Cache.initialize -< call
    (_, NoLoop) -> do
      iterate -< call
  where
    iterate :: c a b
    iterate = proc call -> do
      resultNew <- Stack.push' f -< call
      callInSCC <- SCC.elem -< call
      sccSize <- SCC.size -< ()
      case callInSCC of
        InSCC ptr | sccSize == 1 -> do
          (resultGrown, callNew, resultWidened) <- Cache.update -< (Stable, call, resultNew)
          case resultGrown of
            Stable   -> do
              SCC.remove -< (call, ptr)
              returnA -< resultWidened
            Unstable ->
              iterate -< callNew
        _ -> returnA -< resultNew
{-# INLINE outermost #-}

-- type IterationStrategy c a b = c a b -> c (Stable,a,b) b -> c a b

-- innermost :: (ArrowChoice c, ArrowInComponent a c) => IterationStrategy c a b
-- innermost f iterate = proc a -> do
--   (inComp,b) <- inComponent' f -< a
--   case inComp of
--     Head Outermost -> do
--       iterate -< (Stable,a,b)
--     Head Inner -> do
--       iterate -< (Unstable,a,b)
--     _ -> returnA -< b
-- {-# INLINE innermost #-}
-- {-# SCC innermost #-}

-- innermost' :: (ArrowChoice c, ArrowMetrics a c, ArrowInComponent a c) => IterationStrategy c a b
-- innermost' f iterate = innermost f $ proc (st,a,b) -> do
--   Metrics.iterated -< a
--   iterate -< (st,a,b)
-- {-# INLINE innermost' #-}

-- outermost :: (ArrowChoice c, ArrowInComponent a c) => IterationStrategy c a b
-- outermost f iterate = proc a -> do
--   (inComp,b) <- inComponent' f -< a
--   case inComp of
--     Head Outermost -> do
--       iterate -< (Stable,a,b)
--     Head Inner ->
--       returnA -< b
--     _ ->
--       returnA -< b
-- {-# INLINE outermost #-}
-- {-# SCC outermost #-}

-- outermost' :: (ArrowChoice c, ArrowMetrics a c, ArrowInComponent a c) => IterationStrategy c a b
-- outermost' f iterate = outermost f $ proc (st,a,b) -> do
--   Metrics.iterated -< a
--   iterate -< (st,a,b)
-- {-# INLINE outermost' #-}

-- -- | Iterate on the innermost fixpoint component.
-- chaotic :: forall a b c.
--            (?cacheWidening :: Widening c, ArrowChoice c, ArrowSCC a c, ArrowStack a c, ArrowCache a b c)
--         => IterationStrategy c a b -> FixpointCombinator c a b
-- chaotic iterationStrategy f = proc a -> do
--   m <- Cache.lookup &&& Stack.elem -< a
--   case m of
--     (Just (Stable,b), _) -> returnA -< b
--     (Just (Unstable,b), RecurrentCall ptr) -> do
--       SCC.add -< (a,ptr)
--       returnA -< b
--     (Nothing, RecurrentCall ptr) -> do
--       SCC.add -< (a,ptr)
--       Cache.initialize -< a
--     (_, NoLoop) -> do
--       iterate -< a
--   where
--     iterate :: c a b
--     iterate = iterationStrategy (Stack.push' f) $ proc (stable,a,b) -> do
--       (resultGrown,aNew,bNew) <- Cache.update -< (stable,a,b)
--       case resultGrown of
--         Stable   -> returnA -< bNew
--         Unstable -> iterate -< aNew
--     {-# SCC iterate #-}
--     {-# INLINABLE iterate #-}
-- {-# INLINE chaotic #-}
-- {-# SCC chaotic #-}

