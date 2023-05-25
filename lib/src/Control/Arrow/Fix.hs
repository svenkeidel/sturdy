{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Fix
  (
    ArrowFix(..),
    FixpointAlgorithm,fixpointAlgorithm,transform,
    FixpointCombinator,
    filter,filter',filterPrism,filterPrism',
    recordEvaluated,trace,trace',traceShow,traceCache
  ) where

import           Prelude hiding (filter,pred)

import           Control.Arrow
import           Control.Arrow.Fix.Cache
import           Control.Arrow.Fix.Metrics
import           Control.Arrow.Monad
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.Cokleisli
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Cont
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Writer

import qualified Data.Function as Function
import           Data.Profunctor
import           Data.Lens(Iso',from,get,Prism',getMaybe,set)
import           Prettyprinter

import qualified Debug.Trace as Debug
import           Text.Printf

-- | Interface for describing fixpoint computations.
class ArrowFix c where
  -- | Computes the fixpoint of an arrow computation.
  type Fix c
  fix :: (?fixpointAlgorithm :: FixpointAlgorithm (Fix c)) => (c -> c) -> c

  default fix :: (c ~ c' x y, ArrowLift c', Underlying c' x y ~ d, Fix (c' x y) ~ Fix d, ArrowFix d,
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

filter' :: (ArrowChoice c, ArrowMetrics a c) => (a -> Bool) -> FixpointCombinator c a b -> FixpointCombinator c a b
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

filterPrism' :: forall a a' b c. (ArrowChoice c, ArrowApply c, ArrowMetrics a c) => Prism' a a' -> FixpointCombinator c a' b -> FixpointCombinator c a b
filterPrism' pred strat f = proc a -> case getMaybe pred a of
  Just a' -> strat (lmap (\x -> set pred x a) f) -<< a'
  Nothing -> do
    filtered -< a
    f -< a
{-# INLINE filterPrism' #-}

recordEvaluated :: (ArrowMetrics a c) => FixpointCombinator c a b
recordEvaluated f = proc a -> do
  evaluated -< a
  f -< a
{-# INLINE recordEvaluated #-}

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

traceCache :: ArrowGetCache cache c => (cache -> String) -> FixpointCombinator c a b
traceCache showCache f = proc a -> do
  cache <- getCache -< ()
  b <- f -< Debug.trace (printf "CACHE\n%s\n\n" (showCache cache)) a
  cache' <- getCache -< ()
  returnA -< Debug.trace (printf "CACHE\n%s\n\n" (showCache cache')) b
{-# INLINE traceCache #-}

------------- Instances --------------
instance (ArrowComonad f c, ArrowFix (Underlying (CokleisliT f c) x y)) => ArrowFix (CokleisliT f c x y) where
  type Fix (CokleisliT f c x y) = Fix (Underlying (CokleisliT f c) x y)

instance (Arrow c, Profunctor c, ArrowFix (c x y)) => ArrowFix (ConstT r c x y) where
  type Fix (ConstT r c x y) = Fix (c x y)
  fix f = lift $ \r -> fix (runConstT r . f . lift')
  {-# INLINE fix #-}

instance ArrowFix (c x r) => ArrowFix (ContT r c x y) where
  type Fix (ContT r c x y) = Fix (c x r)
  fix f = lift $ \k -> fix $ \g -> unlift1 f (const g) k
  {-# INLINE fix #-}

instance ArrowFix (c x (f y)) => ArrowFix (KleisliT f c x y) where
  type Fix (KleisliT f c x y) = Fix (Underlying (KleisliT f c) x y)

instance ArrowFix (Underlying (ReaderT r c) x y) => ArrowFix (ReaderT r c x y) where
  type Fix (ReaderT r c x y) = Fix (Underlying (ReaderT r c) x y)

instance ArrowFix (Underlying (StateT s c) x y) => ArrowFix (StateT s c x y) where
  type Fix (StateT s c x y) = Fix (Underlying (StateT s c) x y)

instance ArrowFix (Underlying (WriterT w c) x y) => ArrowFix (WriterT w c x y) where
  type Fix (WriterT w c x y) = Fix (Underlying (WriterT w c) x y)
