{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -DTRACE #-}
module Control.Arrow.Transformer.Abstract.LeastFixPoint(type (~>),runLeastFixPoint,runLeastFixPoint',liftLeastFixPoint) where

import           Prelude hiding (id,(.),lookup)
import           Data.Function (fix)

import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Category

import           Data.Abstract.Terminating
import           Data.Order
import           Data.Identifiable
import           Data.Monoidal

import           Data.Abstract.Error
import           Data.Abstract.Store (Store)
import qualified Data.Abstract.Store as S
import           Data.Abstract.Widening

#ifdef TRACE
import           Debug.Trace
import           Text.Printf
#endif

data (~>) x y
type instance Fix a b (~>) = LeastFixPointArrow a b

-- | Computes the least fixpoint of an arrow computation. The
-- assumption is that the inputs of the computation are finite and the
-- computation is monotone. The inputs of the computation are
-- determined by /all/ layers of the arrow transformer stack, e.g.,
-- a computation of type 'Fix Expr Val (Reader Env (State Store (~>)))'
-- has the inputs 'Expr', 'Env' and 'State'.
--
-- The main idea of the fixpoint algorithm is to cache calls to the
-- interpreter function. Whenever, the interpreter function is called
-- on the same argument for the second time, the cached result is
-- returned instead of continue recursing. A finite domain and
-- monotonicity of the interpreter function ensure the the termination
-- of this algorithm.
-- 
-- This fixpoint caching algorithm is due to- /Abstract Definitional Interpreters, David Darais et. al., ICFP' 17/.
-- We made some changes to the algorithm to simplify it and adjust it to our use case.
newtype LeastFixPointArrow a b x y =
  LeastFixPointArrow (((Store a (Terminating b), Store a (Terminating b)),x) -> (Store a (Terminating b), Terminating y))

runLeastFixPoint :: Fix a b (~>) x y -> (x -> Terminating y)
runLeastFixPoint f = runLeastFixPoint' f >>^ snd

runLeastFixPoint' :: Fix a b (~>) x y -> (x -> (Store a (Terminating b), Terminating y))
runLeastFixPoint' (LeastFixPointArrow f) = (\x -> ((S.empty,S.empty),x)) ^>> f

liftLeastFixPoint :: (x -> y) -> LeastFixPointArrow a b x y
liftLeastFixPoint f = LeastFixPointArrow ((\((_,o),x) -> (o,x)) ^>> second (f ^>> Terminating))


#ifndef TRACE

instance (Identifiable x, Widening y) => ArrowFix x y (LeastFixPointArrow x y) where
  fixA f = proc x -> do
    old <- getOutCache -< ()
    -- reset the current fixpoint cache
    setOutCache -< bottom

    -- recompute the fixpoint cache by calling 'f' and memoize its results.
    y <- localInCache (fix (memoize . f)) -< (old,x)

    new <- getOutCache -< ()

    -- In case the new fixpoint cache contains less information than
    -- the old cache, we are in the reductive set of `f` and have
    -- overshot the fixpoint and stop recursion.  Otherwise, we have
    -- not reached the fixpoint yet and need to continue improving the
    -- fixpoint cache.
    if (new ⊑ old)
    then returnA -< y
    else fixA f -< x

-- | Memoizes the results of the interpreter function. In case a value
-- has been computed before, the cached value is returned and will not
-- be recomputed.
memoize :: (Identifiable x, Widening y) => LeastFixPointArrow x y x y -> LeastFixPointArrow x y x y
memoize (LeastFixPointArrow f) = LeastFixPointArrow $ \((inCache, outCache),x) -> do
  case S.lookup x outCache of
    -- In case the input was in the fixpoint cache, short-cut
    -- recursion and return the cached value.
    Success y -> (outCache,y)

    -- In case the input was not in the fixpoint cache, initialize the
    -- cache with previous knowledge about the result or ⊥, compute
    -- the result of the function and update the fixpoint cache.
    Fail _ ->                
      let yOld = fromError bottom (S.lookup x inCache)
          outCache' = S.insert x yOld outCache
          (outCache'',y) = f ((inCache, outCache'),x)
      in (S.insertWith (flip (▽)) x y outCache'',y)

#else

instance (Show x, Show y, Identifiable x, Widening y)
  => ArrowFix x y (LeastFixPointArrow x y) where
  fixA f = trace (printf "fixA f") $ proc x -> do
    old <- getOutCache -< ()
    setOutCache -< bottom
    y <- localInCache (fix (memoize . f)) -< (old,x)
    new <- getOutCache -< ()
    if new ⊑ old -- We are in the reductive set of `f` and have overshot the fixpoint
    then returnA -< y
    else fixA f -< x

memoize :: (Show x, Show y, Identifiable x, Widening y) => LeastFixPointArrow x y x y -> LeastFixPointArrow x y x y
memoize (LeastFixPointArrow f) = LeastFixPointArrow $ \((inCache, outCache),x) -> do
  case trace (printf "\tmemoize -< %s" (show x)) (S.lookup x outCache) of
    Success y -> trace (printf "\t%s <- memoize -< %s" (show y) (show x)) (outCache,y)
    Fail _ ->
      let yOld = fromError bottom (S.lookup x inCache)
          outCache' = S.insert x yOld outCache
          (outCache'',y) = trace (printf "\tf -< %s" (show x)) (f ((inCache, outCache'),x))
          outCache''' = S.insertWith (flip (▽)) x y outCache''
      in trace (printf "\t%s <- f -< %s\n" (show y) (show x) ++
                printf "\t%s <- memoize -< %s" (show y) (show x))
                  (outCache''',y)

#endif

getOutCache :: LeastFixPointArrow x y () (Store x (Terminating y))
getOutCache = LeastFixPointArrow $ (\((_,o),()) -> (o,return o))

setOutCache :: LeastFixPointArrow x y (Store x (Terminating y)) ()
setOutCache = LeastFixPointArrow $ (\((_,_),o) -> (o,return ()))

localInCache :: LeastFixPointArrow x y x y -> LeastFixPointArrow x y (Store x (Terminating y),x) y
localInCache (LeastFixPointArrow f) = LeastFixPointArrow (\((_,o),(i,x)) -> f ((i,o),x))

instance Category (LeastFixPointArrow i o) where
  id = liftLeastFixPoint id
  LeastFixPointArrow f . LeastFixPointArrow g = LeastFixPointArrow $ proc ((i,o),x) -> do
    (o',y) <- g -< ((i,o),x)
    case y of
      NonTerminating -> returnA -< (o,NonTerminating)
      Terminating y' -> f -< ((i,o'),y')

instance Arrow (LeastFixPointArrow i o) where
  arr f = liftLeastFixPoint (arr f)
  first (LeastFixPointArrow f) = LeastFixPointArrow $ to assoc ^>> first f >>^ (\((o,x'),y) -> (o,strength1 (x',y)))

instance ArrowChoice (LeastFixPointArrow i o) where
  left (LeastFixPointArrow f) = LeastFixPointArrow $ \((i,o),e) -> case e of
    Left x -> second (fmap Left) (f ((i,o),x))
    Right y -> (o,return (Right y))
  right (LeastFixPointArrow f) = LeastFixPointArrow $ \((i,o),e) -> case e of
    Left x -> (o,return (Left x))
    Right y -> second (fmap Right) (f ((i,o),y))
  LeastFixPointArrow f ||| LeastFixPointArrow g = LeastFixPointArrow $ \((i,o),e) -> case e of
    Left x -> f ((i,o),x)
    Right y -> g ((i,o),y)

instance ArrowLoop (LeastFixPointArrow i o) where
  loop (LeastFixPointArrow f) = LeastFixPointArrow $ loop $ \(((i,o),b),d) ->
    case f ((i,o),(b,d)) of
      (o',Terminating (c,d')) -> ((o',Terminating c),d')
      (o',NonTerminating) -> ((o',NonTerminating),d)

instance ArrowApply (LeastFixPointArrow i o) where
  app = LeastFixPointArrow $ (\(io,(LeastFixPointArrow f,x)) -> (f,(io,x))) ^>> app

deriving instance (Identifiable a, PreOrd b, PreOrd y) => PreOrd (LeastFixPointArrow a b x y)
deriving instance (Identifiable a, Complete b, Complete y) => Complete (LeastFixPointArrow a b x y)
deriving instance (Identifiable a, CoComplete b, CoComplete y) => CoComplete (LeastFixPointArrow a b x y)
deriving instance (Identifiable a, PreOrd b, PreOrd y) => LowerBounded (LeastFixPointArrow a b x y)
-- deriving instance (Identifiable a, UpperBounded b, UpperBounded y) => UpperBounded (LeastFixPoint a b x y)
