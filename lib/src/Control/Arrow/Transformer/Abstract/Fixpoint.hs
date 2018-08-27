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
module Control.Arrow.Transformer.Abstract.Fixpoint(Fixpoint,runFix,runFix',runFix'',liftFix) where

import           Prelude hiding (id,(.),lookup)
import qualified Data.Function as F

import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Abstract.Join
import           Control.Category
import           Control.Monad.State hiding (fix)

import           Data.Abstract.Terminating hiding (widening)
import qualified Data.Abstract.Terminating as T
import           Data.Order hiding (lub)
import           Data.Identifiable
import           Data.Monoidal
import           Data.Maybe

import           Data.Abstract.Store (Store)
import qualified Data.Abstract.Store as S
import           Data.Abstract.Widening (Widening)
import qualified Data.Abstract.Widening as W
import           Data.Abstract.StackWidening (StackWidening)
import qualified Data.Abstract.StackWidening as SW

type instance Fix a b (Fixpoint stack () () c) = Fixpoint stack a b (Fix a b c)

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

type Underlying s a b c x y = (StackWidening s a, Widening b) -> c (((s a,Store a (Terminating b)), Store a (Terminating b)),x) (Store a (Terminating b), Terminating y)
newtype Fixpoint s a b c x y = Fixpoint (Underlying s a b c x y)

runFix :: (Arrow c, Complete b) => Fixpoint SW.Unit a b c x y -> c x (Terminating y)
runFix f = runFix' SW.finite W.finite f

runFix' :: (Monoid (s a),Arrow c) => StackWidening s a -> Widening b -> Fixpoint s a b c x y -> c x (Terminating y)
runFix' sw w f = runFix'' sw mempty w f >>^ snd

runFix'' :: Arrow c => StackWidening s a -> s a -> Widening b -> Fixpoint s a b c x y -> c x (Store a (Terminating b), Terminating y)
runFix'' sw s0 w (Fixpoint f) = (\x -> (((s0,S.empty),S.empty),x)) ^>> f (sw,w)

liftLeastFix :: Arrow c => c x y -> Fixpoint s a b c x y
liftLeastFix f = Fixpoint $ \_ -> ((\((_,o),x) -> (o,x)) ^>> second (f >>^ Terminating))

instance (Identifiable x, PreOrd y, ArrowChoice c) => ArrowFix x y (Fixpoint s x y c) where
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
memoize :: (Identifiable x, PreOrd y, ArrowChoice c) => Fixpoint s x y c x y -> Fixpoint s x y c x y
memoize (Fixpoint f) = Fixpoint $ \(stackWidening,widening) -> proc (((stack,inCache), outCache),x) -> do
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
          (x',stack') = runState (stackWidening x) stack 
      (outCache'',y) <- f (stackWidening,widening) -< (((stack',inCache), outCache'),x')
      returnA -< (S.insertWith (flip (T.widening widening)) x y outCache'',y)

getOutCache :: Arrow c => Fixpoint s x y c () (Store x (Terminating y))
getOutCache = Fixpoint $ \_ -> arr $ \((_,o),()) -> (o,return o)

setOutCache :: Arrow c => Fixpoint s x y c (Store x (Terminating y)) ()
setOutCache = Fixpoint $ \_ -> arr $ \((_,_),o) -> (o,return ())

localInCache :: Arrow c => Fixpoint s x y c x y -> Fixpoint s x y c (Store x (Terminating y),x) y
localInCache (Fixpoint f) = Fixpoint $ \w -> proc (((s,_),o),(i,x)) -> f w -< (((s,i),o),x)

instance ArrowChoice c => Category (Fixpoint s i o c) where
  id = liftLeastFix id
  Fixpoint f . Fixpoint g = Fixpoint $ \w -> proc ((i,o),x) -> do
    (o',y) <- g w -< ((i,o),x)
    case y of
      NonTerminating -> returnA -< (o',NonTerminating)
      Terminating y' -> f w -< ((i,o'),y')

instance ArrowChoice c => Arrow (Fixpoint s i o c) where
  arr f = liftLeastFix (arr f)
  first (Fixpoint f) = Fixpoint $ \w -> to assoc ^>> first (f w) >>^ (\((o,x'),y) -> (o,strength1 (x',y)))

instance ArrowChoice c => ArrowChoice (Fixpoint s i o c) where
  left (Fixpoint f) = Fixpoint $ \w -> proc ((i,o),e) -> case e of
    Left x -> second (arr (fmap Left)) . f w -< ((i,o),x)
    Right y -> returnA -< (o,return (Right y))
  right (Fixpoint f) = Fixpoint $ \w -> proc ((i,o),e) -> case e of
    Left x -> returnA -< (o,return (Left x))
    Right y -> second (arr (fmap Right)) . f w -< ((i,o),y)
  Fixpoint f ||| Fixpoint g = Fixpoint $ \w -> proc ((i,o),e) -> case e of
    Left x -> f w -< ((i,o),x)
    Right y -> g w -< ((i,o),y)

instance (ArrowChoice c, ArrowApply c) => ArrowApply (Fixpoint s i o c) where
  app = Fixpoint $ \w -> (\(io,(Fixpoint f,x)) -> (f w,(io,x))) ^>> app

instance (Identifiable i, Complete o, ArrowJoin c, ArrowChoice c) => ArrowJoin (Fixpoint s i o c) where
  joinWith lub (Fixpoint f) (Fixpoint g) = Fixpoint $ \w -> proc ((i,o),(x,y)) -> do
    joinWith (\(o1,t1) (o2,t2) -> (o1 ⊔ o2, case (t1,t2) of
      (Terminating y',Terminating v') -> Terminating (lub y' v')
      (Terminating y',NonTerminating) -> Terminating y'
      (NonTerminating,Terminating v') -> Terminating v'
      (NonTerminating,NonTerminating) -> NonTerminating))
      (f w) (g w) -< (((i,o),x),((i,o),y))

deriving instance (PreOrd (Underlying s a b c x y)) => PreOrd (Fixpoint s a b c x y)
deriving instance (Complete (Underlying s a b c x y)) => Complete (Fixpoint s a b c x y)
deriving instance (CoComplete (Underlying s a b c x y)) => CoComplete (Fixpoint s a b c x y)
deriving instance (LowerBounded (Underlying s a b c x y)) => LowerBounded (Fixpoint s a b c x y)
deriving instance (UpperBounded (Underlying s a b c x y)) => UpperBounded (Fixpoint s a b c x y)
