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
{-# LANGUAGE CPP #-}
module Control.Arrow.Transformer.Abstract.Fixpoint(Fix,FixT,runFixT,runFixT',runFixT'',liftFixT) where

import           Prelude hiding (id,(.),lookup)
import qualified Data.Function as F

import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Trans
import           Control.Arrow.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.Stack
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Static
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Abstract.Join
import           Control.Category
import qualified Control.Monad.State as M

import           Data.Profunctor
import           Data.Order hiding (lub)
import           Data.Identifiable
import           Data.Monoidal
import           Data.Maybe
import           Data.Abstract.Terminating hiding (widening)
import qualified Data.Abstract.Terminating as T
import           Data.Abstract.Map (Map)
import qualified Data.Abstract.Map as M
import           Data.Abstract.Widening (Widening)
import qualified Data.Abstract.Widening as W
import           Data.Abstract.StackWidening (StackWidening)
import qualified Data.Abstract.StackWidening as SW

#ifdef TRACE
import           Debug.Trace
import           Text.Printf
#endif

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
type Cache a b = Map a (Terminating b)
newtype FixT s a b c x y = FixT (ConstT (Widening b) (StackT s a (ReaderT (Cache a b) (TerminatingT (StateT (Cache a b) c)))) x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTerminating)

type instance Fix x y (FixT s () () c) = FixT s x y c

runFixT :: (Complete b, ArrowChoice c, Profunctor c) => FixT SW.Unit a b c x y -> c x (Terminating y)
runFixT f = runFixT' SW.finite W.finite f

runFixT' :: (Monoid (s a),ArrowChoice c, Profunctor c) => StackWidening s a -> Widening b -> FixT s a b c x y -> c x (Terminating y)
runFixT' sw w f = rmap snd (runFixT'' sw w f)

runFixT'' :: (Monoid (s a),ArrowChoice c, Profunctor c) => StackWidening s a -> Widening b -> FixT s a b c x y -> c x (Map a (Terminating b), Terminating y)
runFixT'' sw w (FixT f) =
  lmap (\x -> (M.empty,(M.empty,x))) $
    runStateT
      (runTerminatingT
        (runReaderT
          (runStackT (sw,mempty)
            (runConstT w f))))

liftFixT :: (Arrow c, Profunctor c) => c x y -> FixT s a b c x y
liftFixT = lift'

instance ArrowLift (FixT s a b) where
  lift' f = FixT (ConstT (StaticT (const (StackT (ConstT (StaticT (const (ReaderT (TerminatingT (StateT (lmap snd (second (rmap Terminating f)))))))))))))


#ifndef TRACE
instance (Identifiable x, PreOrd y, ArrowChoice c, Profunctor c) => ArrowFix x y (FixT s x y c) where
  fix f = proc x -> do
    old <- getCache -< ()
    -- reset the current fixpoint cache
    setCache -< bottom

    -- recompute the fixpoint cache by calling 'f' and memoize its results.
    -- y <- localOldCache (F.fix (memoize . f)) -< (old,x)
    y <- localOldCache (F.fix f) -< (old,x)

    new <- getCache -< ()

    -- In case the new fixpoint cache contains less information than
    -- the old cache, we are in the reductive set of `f` and have
    -- overshot the fixpoint and stop recursion.  Otherwise, we have
    -- not reached the fixpoint yet and need to continue improving the
    -- fixpoint cache.
    if {-# SCC "Fix.Cache.comparison" #-}(new ⊑ old)
    then returnA -< y
    else fix f -< x

-- | Memoizes the results of the interpreter function. In case a value
-- has been computed before, the cached value is returned and will not
-- be recomputed.
-- memoize :: (Identifiable x, PreOrd y, ArrowChoice c) => FixT s x y c x y -> FixT s x y c x y
-- memoize (FixT f) = FixT $ \(stackWidening,widening) -> proc (((stack,oldCache), newCache),x) -> do
--   case M.unsafeLookup x newCache of
--     -- In case the input was in the fixpoint cache, short-cut
--     -- recursion and return the cached value.
--     Just y -> returnA -< (newCache,y)

--     -- In case the input was not in the fixpoint cache, initialize the
--     -- cache with previous knowledge about the result or ⊥, compute
--     -- the result of the function and update the fixpoint cache.
--     Nothing -> do
--       let (x',stack') = runState (stackWidening x) stack 
--           yOld        = fromMaybe bottom (M.unsafeLookup x' oldCache)
--           newCache'   = M.insert x' yOld newCache
--       (newCache'',y) <- f (stackWidening,widening) -< (((stack',oldCache), newCache'),x')
--           -- TODO: use insertLookup
--       let newCache''' = M.unsafeInsertWith (flip (T.widening widening)) x' y newCache''
--           y' = fromJust (M.unsafeLookup x' newCache''')
--       returnA -< (newCache''',y')

#else
              
instance (Show x, Show y, Identifiable x, PreOrd y, ArrowChoice c) => ArrowFix x y (FixT s x y c) where
  fix f =  proc x -> do
    old <- getCache -< ()
    setOutCache -< bottom
    y <- localInCache (F.fix (memoize . f)) -< trace "----- ITERATION -----" $ (old,x)
    new <- getOutCache -< ()
    if (new ⊑ old)
    then returnA -< y
    else fix f -< x

memoize :: (Show x, Show y, Identifiable x, PreOrd y, ArrowChoice c) => FixT s x y c x y -> FixT s x y c x y
memoize (FixT f) = FixT $ \(stackWidening,widening) -> proc (((stack,inCache), outCache),x) -> do
  case M.unsafeLookup x outCache of
    Just y -> returnA -< trace (printf "HIT:  %s -> %s" (show x) (show y))
              (outCache,y)
    Nothing -> do
      let yOld = fromMaybe bottom (M.unsafeLookup x inCache)
          outCache' = M.insert x yOld outCache
          (x',stack') = runState (stackWidening x) stack 
      (outCache'',y) <- f (stackWidening,widening) -< trace (printf "CALL: %s" (show x')) (((stack',inCache), outCache'),x')
      let outCache''' = M.unsafeInsertWith (flip (T.widening widening)) x' y outCache''
          y' = fromJust (M.unsafeLookup x' outCache''')
      returnA -< trace (printf "CACHE: %s := (%s -> %s)\n" (show x) (show y) (show y') ++
                        printf "RET:  %s -> %s" (show x') (show y'))
                  (M.unsafeInsertWith (flip (T.widening widening)) x y outCache'',y')
              
#endif

getCache :: (ArrowChoice c, Profunctor c) => FixT s x y c () (Cache x y)
getCache = FixT get

setCache :: (ArrowChoice c, Profunctor c) => FixT s x y c (Map x (Terminating y)) ()
setCache = FixT put

localOldCache :: (ArrowChoice c, Profunctor c) => FixT s x y c x y -> FixT s x y c (Map x (Terminating y),x) y
localOldCache (FixT f) = FixT (local f)

instance (ArrowChoice c, ArrowApply c, Profunctor c) => ArrowApply (FixT s i o c) where
  app = FixT $ lmap (\(FixT f,x) -> (f,x)) app

instance (Identifiable a, ArrowJoin c, ArrowChoice c) => ArrowJoin (FixT s a b c) where
  joinWith lub f g = proc x -> do
    y <- catchTerminating f -< x
    y' <- catchTerminating g -< x
    throwTerminating -< T.widening lub y y'

instance (Identifiable a,Complete y,ArrowJoin c, ArrowChoice c, PreOrd (Underlying a b c x y)) => Complete (FixT s a b c x y) where
  f ⊔ g = joinWith (⊔) f g

type Underlying a b c x y = (c (Map a (Terminating b), (Map a (Terminating b), x)) (Map a (Terminating b), Terminating y))
deriving instance PreOrd (Underlying a b c x y) => PreOrd (FixT s a b c x y)
deriving instance CoComplete (Underlying a b c x y) => CoComplete (FixT s a b c x y)
deriving instance LowerBounded (Underlying a b c x y) => LowerBounded (FixT s a b c x y)
deriving instance UpperBounded (Underlying a b c x y) => UpperBounded (FixT s a b c x y)
