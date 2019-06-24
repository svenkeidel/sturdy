{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Control.Arrow.Transformer.Abstract.Fix(FixT,runFixT) where

import           Prelude hiding (id,(.),const,head,iterate,lookup)

import           Control.Category
import           Control.Arrow hiding (loop)
import           Control.Arrow.Const
import           Control.Arrow.Deduplicate
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Trans
import           Control.Arrow.Abstract.Join
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Static
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State

import           Data.Identifiable
import           Data.Order
import           Data.Profunctor
import           Data.Coerce

import           Data.Abstract.IterationStrategy

newtype FixT stack cache a b c x y = FixT { unFixT ::
  ConstT (IterationStrategy stack cache a b)
    (ReaderT (stack a b) (StateT (cache a b) c)) x y }
  deriving (Profunctor,Category,Arrow,ArrowChoice)


runFixT :: (Identifiable a, PreOrd b, IsEmpty (stack a b), IsEmpty (cache a b), ArrowChoice c, Profunctor c)
  => IterationStrategy stack cache a b -> FixT stack cache a b c x y -> c x y
runFixT iterationStrat (FixT f) =
  dimap (\x -> (empty,(empty,x))) snd $
    runStateT $ runReaderT $ runConstT (iterationStrat) f

type instance Fix x y (FixT stack cache () () c) = FixT stack cache x y c
instance (Identifiable a, LowerBounded b, Profunctor c,ArrowChoice c,ArrowApply c) => ArrowFix a b (FixT stack cache a b c) where
  fix f =
    -- The iteration strategy ensures that the abstract interpreter
    -- terminates, soundly approximates the fixpoint and avoids
    -- unnecessary computation.
    iterationStrategy $ proc strat -> do
                             
    case strat of
      -- The evaluation of some expressions, such as arithmetic
      -- expressions, always terminates. In this case, we simply keep
      -- recursing.
      Compute x ->
        f (fix f) -< x

      -- The evaluation of some other expressions, such as loops, may
      -- may not terminate. Because these expressions may recursively
      -- depend on themselves, we need to iterate on these
      -- expressions, until the result stabilized.
      ComputeAndIterate x upd ->
        let iterate = proc () -> do
              y <- f (fix f) -< x
              r <- update -< (y,upd)
              case r of
                Return y' -> returnA -< y'
                Iterate -> iterate -< ()

         in iterate -<< ()

      -- We may return an unstable result to avoid non-termination
      -- when evaluating an recursive program, Of course this is only
      -- sound if we iterate on the result until it stabilized.
      -- Alternatively, we can avoid redundant evaluation by
      -- returning a cached value instead of recomputing it.
      Cached _ y -> returnA -< y


instance (Identifiable a, ArrowJoin c, ArrowChoice c) => ArrowJoin (FixT stack cache a b c) where
  -- | The join operation of the 'FixT' arrow *does not* join the
  -- cache for efficiency.  This assumes that the cache is extended
  -- only monotonically.  Furthermore, this join operation *is not*
  -- commutative, but it is still associative and computes an upper
  -- bound.
  joinWith lub_ f g = rmap (uncurry lub_) (f &&& g)

instance (ArrowJoin (FixT stack cache a b c), Profunctor c, Complete y, PreOrd (c x y)) => Complete (FixT stack cache a b c x y) where
  f ⊔ g = joinWith (⊔) f g

instance PreOrd (c x y) => PreOrd (FixT stack cache a b c x y) where
  (⊑) = error "f ⊑ g  iff  forall x. snd (f x) ⊑ snd (g x)"

instance (Arrow c, Profunctor c, LowerBounded (c x y)) => LowerBounded (FixT stack cache a b c x y) where
  bottom = lift' bottom

instance ArrowLift (FixT stack cache a b) where
  lift' = FixT . lift' . lift' . lift'

instance (ArrowApply c, Profunctor c) => ArrowApply (FixT stack cache a b c) where
  app = FixT (lmap (first coerce) app)

instance (Arrow c, Profunctor c) => ArrowDeduplicate x y (FixT stack cache a b c) where
  dedup f = f

----- Helper functions -----
iterationStrategy :: (Arrow c,Profunctor c) => FixT stack cache a b c (Compute cache a b) b -> FixT stack cache a b c a b
iterationStrategy (FixT f) = FixT $ askConst $ \iterationStrat ->
  modify (proc (x,cache) -> do
    stack <- ask -< ()
    let (strat,(stack',cache')) = iterationStrat x (stack,cache)
    returnA -< ((stack',strat),cache')
  )
  >>>
  local f

update :: (Identifiable a, Arrow c, Profunctor c) => FixT stack cache a b c (b,Update cache a b) (Iterate b)
update = FixT $ modify' $ \((b,upd),cache) -> upd b cache
{-# INLINE update #-}
