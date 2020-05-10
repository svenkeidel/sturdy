{-# LANGUAGE Arrows #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Fix.Chaotic where

import           Prelude hiding (head,iterate,map)

import           Control.Arrow hiding (loop)
import           Control.Arrow.Fix
import           Control.Arrow.Fix.Metrics as Metrics
import           Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.Fix.Cache as Cache
import           Control.Arrow.Trans

import           Data.Abstract.Stable
import           Data.Profunctor


class (Arrow c, Profunctor c) => ArrowComponent a c where
  addToComponent :: c (a,StackPointer) ()

  default addToComponent :: (c ~ t c', ArrowLift t, ArrowComponent a c') => c (a,StackPointer) ()
  addToComponent = lift' addToComponent
  {-# INLINE addToComponent #-}


data InComponent = Empty | Head Nesting | Body deriving (Show)
data Nesting = Inner | Outermost deriving (Show)

class ArrowComponent a c => ArrowInComponent a c | c -> a where
  inComponent :: c x y -> c (a,x) (InComponent,y)

inComponent' :: ArrowInComponent a c => c a b -> c a (InComponent,b)
inComponent' f = lmap (\a -> (a,a)) (inComponent f)
{-# INLINE inComponent' #-}

type IterationStrategy c a b = c a b -> c (Stable,a,b) b -> c a b

innermost :: (ArrowChoice c, ArrowInComponent a c) => IterationStrategy c a b
innermost f iterate = proc a -> do
  (inComp,b) <- inComponent' f -< a
  case inComp of
    Head Outermost -> do
      iterate -< (Stable,a,b)
    Head Inner -> do
      iterate -< (Unstable,a,b)
    _ -> returnA -< b
{-# INLINE innermost #-}
{-# SCC innermost #-}

innermost' :: (ArrowChoice c, ArrowMetrics a c, ArrowInComponent a c) => IterationStrategy c a b
innermost' f iterate = innermost f $ proc (st,a,b) -> do
  Metrics.iterated -< a
  iterate -< (st,a,b)
{-# INLINE innermost' #-}

outermost :: (ArrowChoice c, ArrowInComponent a c) => IterationStrategy c a b
outermost f iterate = proc a -> do
  (inComp,b) <- inComponent' f -< a
  case inComp of
    Head Outermost -> do
      iterate -< (Stable,a,b)
    Head Inner ->
      returnA -< b
    _ ->
      returnA -< b
{-# INLINE outermost #-}
{-# SCC outermost #-}

outermost' :: (ArrowChoice c, ArrowMetrics a c, ArrowInComponent a c) => IterationStrategy c a b
outermost' f iterate = outermost f $ proc (st,a,b) -> do
  Metrics.iterated -< a
  iterate -< (st,a,b)
{-# INLINE outermost' #-}

-- | Iterate on the innermost fixpoint component.
chaotic :: forall a b c.
           (?cacheWidening :: Widening c, ArrowChoice c, ArrowComponent a c, ArrowStack a c, ArrowCache a b c)
        => IterationStrategy c a b -> FixpointCombinator c a b
chaotic iterationStrategy f = proc a -> do
  m <- Cache.lookup -< a
  case m of
    Just (Stable,b) -> returnA -< b
    _ -> do
      recurrentCall <- Stack.elem -< a
      case recurrentCall of
        RecurrentCall pointer -> do
          addToComponent -< (a,pointer)
          Cache.initialize -< a
        NoLoop -> do
          iterate -< a
  where
    iterate :: c a b
    iterate = iterationStrategy (Stack.push' f) $ proc (stable,a,b) -> do
      (stable',aNew,bNew) <- Cache.update -< (stable,a,b)
      case stable' of
        Stable   -> returnA -< bNew
        Unstable -> iterate -< aNew
    {-# SCC iterate #-}
    {-# INLINABLE iterate #-}
{-# INLINE chaotic #-}
{-# SCC chaotic #-}
