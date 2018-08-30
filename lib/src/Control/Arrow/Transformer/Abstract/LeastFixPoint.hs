{-# LANGUAGE Arrows #-}
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
module Control.Arrow.Transformer.Abstract.LeastFixPoint(LeastFix,runLeastFix,runLeastFix',liftLeastFix) where

import           Prelude hiding (id,(.),lookup)
import qualified Data.Function as F

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

type instance Fix a b (LeastFix () () c) = LeastFix a b (Fix a b c)

-- | Computes the least fixpoint of an arrow computation. The
-- assumption is that the inputs of the computation are finite and the
-- computation is monotone. The inputs of the computation are
-- determined by /all/ layers of the arrow transformer stack, e.g.,
-- a computation of type 'Fix Expr Val (Reader Env (State Store (LeastFix () ())))'
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
type family Underlying (c :: * -> * -> *) x y :: *
type instance Underlying (LeastFix a b c) x y = c ((Store a (Terminating b), Store a (Terminating b)),x) (Store a (Terminating b), Terminating y)

newtype LeastFix a b c x y =
  LeastFix (c ((Store a (Terminating b), Store a (Terminating b)),x) (Store a (Terminating b), Terminating y))

runLeastFix :: Arrow c => LeastFix a b c x y -> c x (Terminating y)
runLeastFix f = runLeastFix' f >>^ snd

runLeastFix' :: Arrow c => LeastFix a b c x y -> (c x (Store a (Terminating b), Terminating y))
runLeastFix' (LeastFix f) = (\x -> ((S.empty,S.empty),x)) ^>> f

liftLeastFix :: Arrow c => c x y -> LeastFix a b c x y
liftLeastFix f = LeastFix ((\((_,o),x) -> (o,x)) ^>> second (f >>^ Terminating))


instance (Identifiable x, Widening y, ArrowChoice c) => ArrowFix x y (LeastFix x y c) where
  fix f = proc x -> do
    old <- getOutCache -< ()
    -- reset the current fixpoint cache
    setOutCache -< bottom

    -- recompute the fixpoint cache by calling 'f' and memoize its results.
    y <- localInCache (F.fix (memoize . f)) -< (old,x)

    new <- getOutCache -< ()

    -- In case the new fixpoint cache contains less information than
    -- the old cache, we are in the reductive set of `f` and have
    -- overshot the fixpoint and stop recursion.  Otherwise, we have
    -- not reached the fixpoint yet and need to continue improving the
    -- fixpoint cache.
    if (new ⊑ old)
    then returnA -< y
    else fix f -< x

-- | Memoizes the results of the interpreter function. In case a value
-- has been computed before, the cached value is returned and will not
-- be recomputed.
memoize :: (Identifiable x, Widening y, ArrowChoice c) => LeastFix x y c x y -> LeastFix x y c x y
memoize (LeastFix f) = LeastFix $ proc ((inCache, outCache),x) -> do
  case S.lookup x outCache of
    -- In case the input was in the fixpoint cache, short-cut
    -- recursion and return the cached value.
    Just y -> returnA -< (outCache,y)

    -- In case the input was not in the fixpoint cache, initialize the
    -- cache with previous knowledge about the result or ⊥, compute
    -- the result of the function and update the fixpoint cache.
    Nothing -> do
      let yOld = fromMaybe bottom (S.lookup x inCache)
          outCache' = S.insert x yOld outCache
      (outCache'',y) <- f -< ((inCache, outCache'),x)
      returnA -< (S.insertWith (flip (▽)) x y outCache'',y)

getOutCache :: Arrow c => LeastFix x y c () (Store x (Terminating y))
getOutCache = LeastFix $ arr $ \((_,o),()) -> (o,return o)

setOutCache :: Arrow c => LeastFix x y c (Store x (Terminating y)) ()
setOutCache = LeastFix $ arr $ \((_,_),o) -> (o,return ())

localInCache :: Arrow c => LeastFix x y c x y -> LeastFix x y c (Store x (Terminating y),x) y
localInCache (LeastFix f) = LeastFix $ proc ((_,o),(i,x)) -> f -< ((i,o),x)

instance ArrowChoice c => Category (LeastFix i o c) where
  id = liftLeastFix id
  LeastFix f . LeastFix g = LeastFix $ proc ((i,o),x) -> do
    (o',y) <- g -< ((i,o),x)
    case y of
      NonTerminating -> returnA -< (o',NonTerminating)
      Terminating y' -> f -< ((i,o'),y')

instance ArrowChoice c => Arrow (LeastFix i o c) where
  arr f = liftLeastFix (arr f)
  first (LeastFix f) = LeastFix $ to assoc ^>> first f >>^ (\((o,x'),y) -> (o,strength1 (x',y)))

instance ArrowChoice c => ArrowChoice (LeastFix i o c) where
  left (LeastFix f) = LeastFix $ proc ((i,o),e) -> case e of
    Left x -> second (arr (fmap Left)) . f -< ((i,o),x)
    Right y -> returnA -< (o,return (Right y))
  right (LeastFix f) = LeastFix $ proc ((i,o),e) -> case e of
    Left x -> returnA -< (o,return (Left x))
    Right y -> second (arr (fmap Right)) . f -< ((i,o),y)
  LeastFix f ||| LeastFix g = LeastFix $ proc ((i,o),e) -> case e of
    Left x -> f -< ((i,o),x)
    Right y -> g -< ((i,o),y)

instance (ArrowChoice c, ArrowApply c) => ArrowApply (LeastFix i o c) where
  app = LeastFix $ (\(io,(LeastFix f,x)) -> (f,(io,x))) ^>> app

instance (Identifiable i, Complete o, ArrowJoin c, ArrowChoice c) => ArrowJoin (LeastFix i o c) where
  joinWith lub (LeastFix f) (LeastFix g) = LeastFix $ proc ((i,o),(x,y)) -> do
    joinWith (\(o1,t1) (o2,t2) -> (o1 ⊔ o2, case (t1,t2) of
      (Terminating y',Terminating v') -> Terminating (lub y' v')
      (Terminating y',NonTerminating) -> Terminating y'
      (NonTerminating,Terminating v') -> Terminating v'
      (NonTerminating,NonTerminating) -> NonTerminating))
      f g -< (((i,o),x),((i,o),y))

deriving instance (PreOrd (Underlying (LeastFix a b c) x y)) => PreOrd (LeastFix a b c x y)
deriving instance (Complete (Underlying (LeastFix a b c) x y)) => Complete (LeastFix a b c x y)
deriving instance (CoComplete (Underlying (LeastFix a b c) x y)) => CoComplete (LeastFix a b c x y)
deriving instance (LowerBounded (Underlying (LeastFix a b c) x y)) => LowerBounded (LeastFix a b c x y)
deriving instance (UpperBounded (Underlying (LeastFix a b c) x y)) => UpperBounded (LeastFix a b c x y)
