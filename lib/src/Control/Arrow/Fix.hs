{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Fix
  (
    ArrowFix(..),
    FixpointAlgorithm,fixpointAlgorithm,transform,
    FixpointCombinator,
    filter,filter',filterPrism,filterPrism',
    trace,trace',traceShow,traceCache
  ) where

import           Prelude hiding (filter,pred)

import           Control.Arrow
import           Control.Arrow.State (ArrowState)
import qualified Control.Arrow.State as State
import           Control.Arrow.Trans
import           Control.Arrow.Fix.Metrics

import qualified Data.Function as Function
import           Data.Profunctor
import           Data.Lens(Iso',from,get,Prism',getMaybe,set)
import           Data.Text.Prettyprint.Doc

import qualified Debug.Trace as Debug
import           Text.Printf

-- | Interface for describing fixpoint computations.
class ArrowFix c where
  -- | Computes the fixpoint of an arrow computation.
  type Fix c
  fix :: (?fixpointAlgorithm :: FixpointAlgorithm (Fix c)) => (c -> c) -> c

  default fix :: (c ~ c' x y, ArrowTrans c', Underlying c' x y ~ d, Fix (c' x y) ~ Fix d, ArrowFix d,
                 ?fixpointAlgorithm :: FixpointAlgorithm (Fix c))
              => (c -> c) -> c
  fix f = lift (fix (unlift . f . lift))
  {-# INLINE fix #-}

instance ArrowFix (x -> y) where
  type Fix (x -> y) = (x -> y)
  fix = ?fixpointAlgorithm
  {-# INLINE fix #-}

type FixpointAlgorithm c = (c -> c) -> c

transform :: Profunctor c => Iso' a a' -> Iso' b b' -> FixpointAlgorithm (c a' b') -> FixpointAlgorithm (c a b)
transform isoA isoB algorithm eval = toIso $ algorithm (fromIso . eval . toIso)
  where
    fromIso = dimap (get (from isoA)) (get isoB)
    toIso = dimap (get isoA) (get (from isoB))
    {-# INLINE fromIso #-}
    {-# INLINE toIso #-}
{-# INLINE transform #-}

type FixpointCombinator c a b = c a b -> c a b

fixpointAlgorithm :: FixpointCombinator c x y -> FixpointAlgorithm (c x y)
fixpointAlgorithm combinator eval = Function.fix (combinator . eval)
{-# INLINE fixpointAlgorithm #-}

filter :: ArrowChoice c => (a -> Bool) -> FixpointCombinator c a b -> FixpointCombinator c a b
filter pred combinator f = proc a ->
  if pred a
  then combinator f -< a
  else f -< a
{-# INLINE filter #-}

filter' :: (ArrowChoice c, ArrowFiltered a c) => (a -> Bool) -> FixpointCombinator c a b -> FixpointCombinator c a b
filter' pred combinator f = proc a ->
  if pred a
  then combinator f -< a
  else do
    filtered -< a
    f -< a
{-# INLINE filter' #-}

filterPrism :: forall a a' b c. (Profunctor c, ArrowChoice c, ArrowApply c) => Prism' a a' -> FixpointCombinator c a' b -> FixpointCombinator c a b
filterPrism pred combinator f = proc a -> case getMaybe pred a of
  Just a' -> combinator (lmap (\x -> set pred x a) f) -<< a'
  Nothing -> f -< a
{-# INLINE filterPrism #-}

filterPrism' :: forall a a' b c. (ArrowChoice c, ArrowApply c, ArrowFiltered a c) => Prism' a a' -> FixpointCombinator c a' b -> FixpointCombinator c a b
filterPrism' pred strat f = proc a -> case getMaybe pred a of
  Just a' -> strat (lmap (\x -> set pred x a) f) -<< a'
  Nothing -> do
    filtered -< a
    f -< a
{-# INLINE filterPrism' #-}

trace :: (Arrow c) => (a -> Doc ann) -> (b -> Doc ann) -> FixpointCombinator c a b
trace showA showB f = proc x -> do
  y <- f -< Debug.trace (show (vsep ["CALL", showA x, line])) x
  returnA -< Debug.trace (show (vsep ["RETURN", showA x, showB y,line])) y
{-# INLINE trace #-}

trace' :: (Eq a, ArrowApply c) => (a -> String) -> (b -> String) -> FixpointCombinator c a b -> FixpointCombinator c a b
trace' showA showB strat f = proc x -> do
  y <- strat (proc x' -> f -< Debug.trace (if x == x'
                                           then printf "CALL\n%s\n\n" (showA x)
                                           else printf "CALL\n%s~>\n%s\n\n" (showA x) (showA x')) x') -<< x
  returnA -< Debug.trace (printf "RETURN\n%s\n%s\n\n" (showA x) (showB y)) y
{-# INLINE trace' #-}

traceShow :: (Pretty a, Pretty b, Arrow c) => FixpointCombinator c a b
traceShow = trace pretty pretty
{-# INLINE traceShow #-}

traceCache :: ArrowState cache c => (cache -> String) -> FixpointCombinator c a b
traceCache showCache f = proc a -> do
  cache <- State.get -< ()
  b <- f -< Debug.trace (printf "CACHE %s\n\n" (showCache cache)) a
  cache' <- State.get -< ()
  returnA -< Debug.trace (printf "CACHE %s\n\n" (showCache cache')) b
{-# INLINE traceCache #-}
