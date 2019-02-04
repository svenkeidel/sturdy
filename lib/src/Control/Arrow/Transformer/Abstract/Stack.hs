{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Stack(StackT(..),runStackT,runStackT',stackT) where

import           Prelude hiding ((.))

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Trans
import           Control.Arrow.State
import           Control.Arrow.Reader
import           Control.Arrow.Const
import           Control.Arrow.Abstract.Join
import           Control.Arrow.Abstract.Terminating
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Static
import           Control.Arrow.Transformer.Reader

import qualified Control.Monad.State as M

import           Data.Order
import           Data.Profunctor
import           Data.Abstract.StackWidening (StackWidening)

newtype StackT s a c x y = StackT { unStackT :: ConstT (StackWidening s a) (ReaderT (s a) c) x y }
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowJoin, ArrowState r,ArrowTerminating)

runStackT :: (Arrow c, Profunctor c) => (StackWidening s a,s a) -> StackT s a c x y -> c x y
runStackT (w,s) (StackT f) = lmap (s,) (runReaderT (runConstT w f))

runStackT' :: (Arrow c, Profunctor c, Monoid (s a)) => StackWidening s a -> StackT s a c x y -> c x y
runStackT' s = runStackT (s,mempty)

stackT :: (StackWidening s a -> c (s a,x) y) -> StackT s a c x y
stackT f = StackT $ ConstT $ StaticT $ \w -> ReaderT (f w)

type instance Fix x y (StackT s () c) = StackT s x (Fix x y c)
instance (Profunctor c,ArrowFix (s x,x) y c) => ArrowFix x y (StackT s x c) where
  fix f = StackT $ fix (unStackT . widen . f . StackT)
    where
      widen :: (Arrow c, Profunctor c) => StackT s x c x y -> StackT s x c x y
      widen (StackT f') = proc x -> do
        stackWiden <- StackT askConst -< ()
        stack <- StackT ask -< ()
        let (x',stack') = M.runState (stackWiden x) stack
        StackT (local f') -< (stack',x')

instance (ArrowApply c,Profunctor c) => ArrowApply (StackT s a c) where
  app = StackT (lmap (\(StackT f,x) -> (f,x)) app)

instance ArrowLift (StackT s a) where
  lift' = StackT . lift' . lift'

deriving instance PreOrd (c (s a,x) y) => PreOrd (StackT s a c x y)
deriving instance Complete (c (s a,x) y) => Complete (StackT s a c x y)
deriving instance CoComplete (c (s a,x) y) => CoComplete (StackT s a c x y)
deriving instance LowerBounded (c (s a,x) y) => LowerBounded (StackT s a c x y)
deriving instance UpperBounded (c (s a,x) y) => UpperBounded (StackT s a c x y)
