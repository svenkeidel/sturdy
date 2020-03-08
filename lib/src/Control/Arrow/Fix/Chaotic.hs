{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Fix.Chaotic where

import           Prelude hiding (head,iterate,map)

import           Control.Arrow hiding (loop)
import           Control.Arrow.Fix
import           Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.Fix.Cache as Cache
import           Control.Arrow.Trans

import           Data.Abstract.Stable
import           Data.Profunctor


class (Arrow c, Profunctor c) => ArrowComponent a c | c -> a where
  addToComponent :: c a ()
  removeFromComponent :: c a ()

  default addToComponent :: (c ~ t c', ArrowLift t, ArrowComponent a c') => c a ()
  default removeFromComponent :: (c ~ t c', ArrowLift t, ArrowComponent a c') => c a ()

  addToComponent = lift' addToComponent
  removeFromComponent = lift' removeFromComponent

  {-# INLINE addToComponent #-}
  {-# INLINE removeFromComponent #-}


data InComponent = Empty | Head Nesting | Body
data Nesting = Inner | Outermost

class ArrowComponent a c => ArrowInComponent a c where
  inComponent :: c a InComponent

  default inComponent :: (c ~ t c', ArrowLift t, ArrowInComponent a c') => c a InComponent
  inComponent = lift' inComponent
  {-# INLINE inComponent #-}

type IterationStrategy c a b = c a b -> c (a,b) b -> c a b

innermost :: (ArrowChoice c, ArrowInComponent a c) => IterationStrategy c a b
innermost f iterate = proc a -> do
  b <- f -< a
  inComp <- inComponent -< a; case inComp of
    Head _ -> do
      removeFromComponent -< a
      iterate -< (a,b)
    _ ->
      returnA -< b
{-# INLINE innermost #-}

outermost :: (ArrowChoice c, ArrowInComponent a c) => IterationStrategy c a b
outermost f iterate = proc a -> do
  b <- f -< a
  inComp <- inComponent -< a; case inComp of
    Head Outermost -> do
      removeFromComponent -< a
      iterate -< (a,b)
    Head _ -> do
      removeFromComponent -< a
      returnA -< b
    _ ->
      returnA -< b
{-# INLINE outermost #-}

-- | Iterate on the innermost fixpoint component.
chaotic :: forall a b c. (ArrowChoice c, ArrowComponent a c, ArrowStack a c, ArrowCache a b c)
        => IterationStrategy c a b -> FixpointCombinator c a b
chaotic iterationStrategy f = proc a -> do
  loop <- Stack.elem -< a
  if loop
  then do
    m <- Cache.lookup -< a
    case m of
      Just (Stable,b) -> returnA -< b
      Just (Unstable,b) -> do
        addToComponent -< a
        returnA -< b
      Nothing -> do
        b <- Cache.initialize -< a
        addToComponent -< a
        returnA -< b
  else
    iterate -< a
  where
    iterate = iterationStrategy (Stack.push f) $ proc (a,b) -> do
      (stable,aNew,bNew) <- Cache.update -< (a,b)
      case stable of
        Stable   -> returnA -< bNew
        Unstable -> iterate -< aNew
{-# INLINE chaotic #-}
