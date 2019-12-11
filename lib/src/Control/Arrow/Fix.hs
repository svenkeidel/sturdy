{-# LANGUAGE Arrows #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Fix(Fix,Fix',ArrowFix(..),FixpointCombinator,transform,filter,trace,trace',traceShow,traceCache) where

import Prelude hiding (filter,pred)

import Control.Arrow
import Control.Arrow.State (ArrowState)
import qualified Control.Arrow.State as State
import Control.Arrow.Trans
import Data.Profunctor
import Data.Lens(Iso',from,get,Prism',getMaybe,set)

import qualified Debug.Trace as Debug
import Text.Printf

-- | Type family that computes the type of the fixpoint.
type family Fix (c :: * -> * -> *) x y :: * -> * -> *
type Fix' c x y = Fix c x y x y

-- | Interface for describing fixpoint computations.
class ArrowFix c where
  -- | Computes the fixpoint of an arrow computation.
  fix :: (c -> c) -> c

  default fix :: (c ~ c' x y, ArrowTrans c', Underlying c' x y ~ c'' x' y', ArrowFix (c'' x' y')) => (c -> c) -> c
  fix f = lift (fix (unlift . f . lift))
  {-# INLINE fix #-}

type instance Fix (->) x y = (->)
instance ArrowFix (x -> y) where
  fix f = f (fix f)

type FixpointCombinator c x y = c x y -> c x y

transform :: Profunctor c => Iso' a a' -> FixpointCombinator c a' b -> FixpointCombinator c a b
transform iso strat f = lmap (get iso)
                      $ strat
                      $ lmap (get (from iso)) f
{-# INLINE transform #-}

filter :: forall a a' b c. (Profunctor c, ArrowChoice c, ArrowApply c) => Prism' a a' -> FixpointCombinator c a' b -> FixpointCombinator c a b
filter pred strat f = proc a -> case getMaybe pred a of
  Just a' -> strat (lmap (\x -> set pred x a) f) -<< a'
  Nothing -> f -< a
{-# INLINE filter #-}

trace :: (Arrow c) => (a -> String) -> (b -> String) -> FixpointCombinator c a b
trace showA showB f = proc x -> do
  y <- f -< Debug.trace (printf "CALL\n%s\n\n" (showA x)) x
  returnA -< Debug.trace (printf "RETURN\n%s\n%s\n\n" (showA x) (showB y)) y
{-# INLINE trace #-}

trace' :: (Eq a, ArrowApply c) => (a -> String) -> (b -> String) -> FixpointCombinator c a b -> FixpointCombinator c a b
trace' showA showB strat f = proc x -> do
  y <- strat (proc x' -> f -< Debug.trace (if x == x'
                                           then printf "CALL\n%s\n\n" (showA x)
                                           else printf "CALL\n%s~>\n%s\n\n" (showA x) (showA x')) x') -<< x
  returnA -< Debug.trace (printf "RETURN\n%s\n%s\n\n" (showA x) (showB y)) y
{-# INLINE trace' #-}

traceShow :: (Show a, Show b, Arrow c) => FixpointCombinator c a b
traceShow = trace show show
{-# INLINE traceShow #-}

traceCache :: ArrowState cache c => (cache -> String) -> FixpointCombinator c a b
traceCache showCache f = proc a -> do
  cache <- State.get -< ()
  b <- f -< Debug.trace (printf "CACHE %s\n\n" (showCache cache)) a
  cache' <- State.get -< ()
  returnA -< Debug.trace (printf "CACHE %s\n\n" (showCache cache')) b
{-# INLINE traceCache #-}
