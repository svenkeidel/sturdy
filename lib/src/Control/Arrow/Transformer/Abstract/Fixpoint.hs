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
module Control.Arrow.Transformer.Abstract.Fixpoint(FixT,runFixT,runFixT',runFixT'',liftFixT) where

import           Prelude hiding (id,(.),lookup)
import qualified Data.Function as F

import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Abstract.Join
import           Control.Category
import           Control.Monad.State hiding (fix)

import           Data.Abstract.Terminating hiding (widening)
import qualified Data.Abstract.Terminating as T
import           Data.Abstract.Map (Map)
import qualified Data.Abstract.Map as M
import           Data.Order hiding (lub)
import           Data.Identifiable
import           Data.Monoidal
import           Data.Maybe

import           Data.Abstract.Widening (Widening)
import qualified Data.Abstract.Widening as W
import           Data.Abstract.StackWidening (StackWidening)
import qualified Data.Abstract.StackWidening as SW

-- | Fixpoint algorithm that computes the least fixpoint of an arrow computation.
-- This fixpoint caching algorithm is due to /Abstract Definitional
-- Interpreters, David Darais et. al., ICFP' 17/.  We made some
-- changes to the algorithm to simplify it and adjust it to our use
-- case.  The main idea of the fixpoint algorithm is to cache calls to
-- the interpreter function. Whenever, the interpreter function is
-- called on the same argument for the second time, the cached result
-- is returned instead of continue recursing. Widening of the domain
-- and codomain and monotonicity of the interpreter function ensure
-- the the termination of this algorithm.
--
-- Because of the caching of calls interpreter calls, 'LeastFix stack a b'
-- needs to know precisely the type of the domain and codomain of the
-- interpreter, which is captured in the type parameters 'a' and 'b'.
-- What 'a' and 'b' have to be is determined by /all/ layers of the
-- arrow transformer stack. For instance, in the arrow-transformer
-- stack 'Reader Env (State Store (LeastFix stack a b))' the input
-- type 'a' contains 'Env' and 'Store' and the output type only
-- contains 'Store'.  These types can be computed automatically with
-- the type family 'Fix'. For example, the type expression
-- 'Fix Expr Val (Reader Env (State Store (LeastFix Stack () ())))'
-- evaluates to
-- 'Reader Env (State Store (LeastFix Stack (Store,(Env,Expr)) (Store)))'

type Underlying s a b c x y = (StackWidening s a, Widening b) -> c (((s a,Map a (Terminating b)), Map a (Terminating b)),x) (Map a (Terminating b), Terminating y)
newtype FixT s a b c x y = FixT (Underlying s a b c x y)
type instance Fix a b (FixT stack () () c) = FixT stack a b (Fix a b c)

runFixT :: (Arrow c, Complete b) => FixT SW.Unit a b c x y -> c x (Terminating y)
runFixT f = runFixT' SW.finite W.finite f

runFixT' :: (Monoid (s a),Arrow c) => StackWidening s a -> Widening b -> FixT s a b c x y -> c x (Terminating y)
runFixT' sw w f = runFixT'' sw w f >>^ snd

runFixT'' :: (Monoid (s a),Arrow c) => StackWidening s a -> Widening b -> FixT s a b c x y -> c x (Map a (Terminating b), Terminating y)
runFixT'' sw w (FixT f) = (\x -> (((mempty,M.empty),M.empty),x)) ^>> f (sw,w)

liftFixT :: Arrow c => c x y -> FixT s a b c x y
liftFixT f = FixT $ \_ -> ((\((_,o),x) -> (o,x)) ^>> second (f >>^ Terminating))

instance (Identifiable x, PreOrd y, ArrowChoice c) => ArrowFix x y (FixT s x y c) where
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
memoize :: (Identifiable x, PreOrd y, ArrowChoice c) => FixT s x y c x y -> FixT s x y c x y
memoize (FixT f) = FixT $ \(stackWidening,widening) -> proc (((stack,inCache), outCache),x) -> do
  case M.unsafeLookup x outCache of
    -- In case the input was in the fixpoint cache, short-cut
    -- recursion and return the cached value.
    Just y -> returnA -< (outCache,y)

    -- In case the input was not in the fixpoint cache, initialize the
    -- cache with previous knowledge about the result or ⊥, compute
    -- the result of the function and update the fixpoint cache.
    Nothing -> do
      let yOld = fromMaybe bottom (M.unsafeLookup x inCache)
          outCache' = M.insert x yOld outCache
          (x',stack') = runState (stackWidening x) stack 
      (outCache'',y) <- f (stackWidening,widening) -< (((stack',inCache), outCache'),x')
      returnA -< (M.unsafeInsertWith (flip (T.widening widening)) x y outCache'',y)

getOutCache :: Arrow c => FixT s x y c () (Map x (Terminating y))
getOutCache = FixT $ \_ -> arr $ \((_,o),()) -> (o,return o)

setOutCache :: Arrow c => FixT s x y c (Map x (Terminating y)) ()
setOutCache = FixT $ \_ -> arr $ \((_,_),o) -> (o,return ())

localInCache :: Arrow c => FixT s x y c x y -> FixT s x y c (Map x (Terminating y),x) y
localInCache (FixT f) = FixT $ \w -> proc (((s,_),o),(i,x)) -> f w -< (((s,i),o),x)

instance ArrowChoice c => Category (FixT s i o c) where
  id = liftFixT id
  FixT f . FixT g = FixT $ \w -> proc ((i,o),x) -> do
    (o',y) <- g w -< ((i,o),x)
    case y of
      NonTerminating -> returnA -< (o',NonTerminating)
      Terminating y' -> f w -< ((i,o'),y')

instance ArrowChoice c => Arrow (FixT s i o c) where
  arr f = liftFixT (arr f)
  first (FixT f) = FixT $ \w -> to assoc ^>> first (f w) >>^ (\((o,x'),y) -> (o,strength1 (x',y)))

instance ArrowChoice c => ArrowChoice (FixT s i o c) where
  left (FixT f) = FixT $ \w -> proc ((i,o),e) -> case e of
    Left x -> second (arr (fmap Left)) . f w -< ((i,o),x)
    Right y -> returnA -< (o,return (Right y))
  right (FixT f) = FixT $ \w -> proc ((i,o),e) -> case e of
    Left x -> returnA -< (o,return (Left x))
    Right y -> second (arr (fmap Right)) . f w -< ((i,o),y)
  FixT f ||| FixT g = FixT $ \w -> proc ((i,o),e) -> case e of
    Left x -> f w -< ((i,o),x)
    Right y -> g w -< ((i,o),y)

instance (ArrowChoice c, ArrowApply c) => ArrowApply (FixT s i o c) where
  app = FixT $ \w -> (\(io,(FixT f,x)) -> (f w,(io,x))) ^>> app

instance (Identifiable i, Complete o, ArrowJoin c, ArrowChoice c) => ArrowJoin (FixT s i o c) where
  joinWith lub (FixT f) (FixT g) = FixT $ \w -> proc ((i,o),(x,y)) -> do
    (o',t1) <- f w -< ((i,o),x)
    (o'',t2) <- g w -< ((i,o'),y)
    returnA -< (o'',case (t1,t2) of
      (Terminating y',Terminating v') -> Terminating (lub y' v')
      (Terminating y',NonTerminating) -> Terminating y'
      (NonTerminating,Terminating v') -> Terminating v'
      (NonTerminating,NonTerminating) -> NonTerminating)

deriving instance (PreOrd (Underlying s a b c x y)) => PreOrd (FixT s a b c x y)
instance (Arrow c,PreOrd (Underlying s a b c x y),Complete y) => Complete (FixT s a b c x y) where
  FixT f ⊔ FixT g = FixT $ \w -> proc ((i,o),x) -> do
    (o',t1) <- f w -< ((i,o),x)
    (o'',t2) <- g w -< ((i,o'),x)
    returnA -< (o'',case (t1,t2) of
      (Terminating y',Terminating v') -> Terminating (y' ⊔ v')
      (Terminating y',NonTerminating) -> Terminating y'
      (NonTerminating,Terminating v') -> Terminating v'
      (NonTerminating,NonTerminating) -> NonTerminating)

deriving instance (CoComplete (Underlying s a b c x y)) => CoComplete (FixT s a b c x y)
deriving instance (LowerBounded (Underlying s a b c x y)) => LowerBounded (FixT s a b c x y)
deriving instance (UpperBounded (Underlying s a b c x y)) => UpperBounded (FixT s a b c x y)
