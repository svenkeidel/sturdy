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
import           Control.Arrow.Abstract.Join
import           Control.Category

import           Data.Abstract.Terminating
import           Data.Order hiding (lub)
import           Data.Identifiable
import           Data.Monoidal
import           Data.Maybe

import           Data.Abstract.Store (Store)
import qualified Data.Abstract.Store as S
import           Data.Abstract.Widening

#ifdef TRACE
import           Debug.Trace
import           Text.Printf
#endif

data (~>) x y
type instance Fix a b (~>) = LeastFixPoint a b

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
newtype LeastFixPoint a b x y =
  LeastFixPoint (((Store a (Terminating b), Store a (Terminating b)),x) -> (Store a (Terminating b), Terminating y))

runLeastFixPoint :: Fix a b (~>) x y -> (x -> Terminating y)
runLeastFixPoint f = runLeastFixPoint' f >>^ snd

runLeastFixPoint' :: Fix a b (~>) x y -> (x -> (Store a (Terminating b), Terminating y))
runLeastFixPoint' (LeastFixPoint f) = (\x -> ((S.empty,S.empty),x)) ^>> f

liftLeastFixPoint :: (x -> y) -> LeastFixPoint a b x y
liftLeastFixPoint f = LeastFixPoint ((\((_,o),x) -> (o,x)) ^>> second (f ^>> Terminating))


#ifndef TRACE

instance (Identifiable x, Widening y) => ArrowFix x y (LeastFixPoint x y) where
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
memoize :: (Identifiable x, Widening y) => LeastFixPoint x y x y -> LeastFixPoint x y x y
memoize (LeastFixPoint f) = LeastFixPoint $ \((inCache, outCache),x) -> do
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
  => ArrowFix x y (LeastFixPoint x y) where
  fixA f = trace (printf "fixA f") $ proc x -> do
    old <- getOutCache -< ()
    setOutCache -< bottom
    y <- localInCache (fix (memoize . f)) -< (old,x)
    new <- getOutCache -< ()
    if new ⊑ old -- We are in the reductive set of `f` and have overshot the fixpoint
    then returnA -< y
    else fixA f -< x

memoize :: (Show x, Show y, Identifiable x, Widening y)
        => LeastFixPoint x y x y -> LeastFixPoint x y x y
memoize (LeastFixPoint f) = LeastFixPoint $ \((inCache, outCache),x) -> do
  case trace (printf "\tmemoize -< %s" (show x)) (S.lookup x outCache) of
    Just y -> trace (printf "\t%s <- memoize -< %s" (show y) (show x)) (outCache,y)
    Nothing ->
      let yOld = fromMaybe bottom (S.lookup x inCache)
          outCache' = S.insert x yOld outCache
          (outCache'',y) = trace (printf "\tf -< %s" (show x)) (f ((inCache, outCache'),x))
          outCache''' = S.insertWith (flip (▽)) x y outCache''
      in trace (printf "\t%s <- f -< %s\n" (show y) (show x) ++
                printf "\t%s <- memoize -< %s" (show y) (show x))
                  (outCache''',y)

#endif

getOutCache :: LeastFixPoint x y () (Store x (Terminating y))
getOutCache = LeastFixPoint $ (\((_,o),()) -> (o,return o))

setOutCache :: LeastFixPoint x y (Store x (Terminating y)) ()
setOutCache = LeastFixPoint $ (\((_,_),o) -> (o,return ()))

localInCache :: LeastFixPoint x y x y -> LeastFixPoint x y (Store x (Terminating y),x) y
localInCache (LeastFixPoint f) = LeastFixPoint (\((_,o),(i,x)) -> f ((i,o),x))

instance Category (LeastFixPoint i o) where
  id = liftLeastFixPoint id
  LeastFixPoint f . LeastFixPoint g = LeastFixPoint $ proc ((i,o),x) -> do
    (o',y) <- g -< ((i,o),x)
    case y of
      NonTerminating -> returnA -< (o,NonTerminating)
      Terminating y' -> f -< ((i,o'),y')

instance Arrow (LeastFixPoint i o) where
  arr f = liftLeastFixPoint (arr f)
  first (LeastFixPoint f) = LeastFixPoint $ to assoc ^>> first f >>^ (\((o,x'),y) -> (o,strength1 (x',y)))

instance ArrowChoice (LeastFixPoint i o) where
  left (LeastFixPoint f) = LeastFixPoint $ \((i,o),e) -> case e of
    Left x -> second (fmap Left) (f ((i,o),x))
    Right y -> (o,return (Right y))
  right (LeastFixPoint f) = LeastFixPoint $ \((i,o),e) -> case e of
    Left x -> (o,return (Left x))
    Right y -> second (fmap Right) (f ((i,o),y))
  LeastFixPoint f ||| LeastFixPoint g = LeastFixPoint $ \((i,o),e) -> case e of
    Left x -> f ((i,o),x)
    Right y -> g ((i,o),y)

instance ArrowLoop (LeastFixPoint i o) where
  loop (LeastFixPoint f) = LeastFixPoint $ loop $ \(((i,o),b),d) ->
    case f ((i,o),(b,d)) of
      (o',Terminating (c,d')) -> ((o',Terminating c),d')
      (o',NonTerminating) -> ((o',NonTerminating),d)

instance ArrowApply (LeastFixPoint i o) where
  app = LeastFixPoint $ (\(io,(LeastFixPoint f,x)) -> (f,(io,x))) ^>> app

instance (Identifiable i, Complete o) => ArrowJoin (LeastFixPoint i o) where
  joinWith lub (LeastFixPoint f) (LeastFixPoint g) = LeastFixPoint $ \((i,o),(x,u)) ->
    let (o',y) = f ((i,o),x)
        (o'',v) = g ((i,o),u)
    in (o' ⊔ o'',case (y,v) of
         (Terminating y',Terminating v') -> Terminating (lub y' v')
         (Terminating y',NonTerminating) -> Terminating y'
         (NonTerminating,Terminating v') -> Terminating v'
         (NonTerminating,NonTerminating) -> NonTerminating) 

deriving instance (Identifiable a, PreOrd b, PreOrd y) => PreOrd (LeastFixPoint a b x y)
-- TODO: Figure out if it is sound to thread the fixpoint cache while computing the least upper bound of two arrow computations.
deriving instance (Identifiable a, Complete b, Complete y) => Complete (LeastFixPoint a b x y)
deriving instance (Identifiable a, CoComplete b, CoComplete y) => CoComplete (LeastFixPoint a b x y)
deriving instance (Identifiable a, PreOrd b, PreOrd y) => LowerBounded (LeastFixPoint a b x y)
-- deriving instance (Identifiable a, UpperBounded b, UpperBounded y) => UpperBounded (LeastFixPoint a b x y)
