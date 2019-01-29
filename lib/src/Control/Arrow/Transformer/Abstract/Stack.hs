{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Abstract.Stack(StackT,runStackT,runStackT') where

import           Prelude hiding ((.))

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Trans
import           Control.Arrow.Abstract.Join
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Static

import qualified Control.Monad.State as M

import           Data.Order
import           Data.Abstract.StackWidening (StackWidening)

newtype StackT s a c x y = StackT (ConstT (StackWidening s a,s a) c x y)
  deriving (Category,Arrow,ArrowChoice,ArrowLift,ArrowJoin,PreOrd,Complete)
instance ArrowApply c => ArrowApply (StackT s a c) where app = StackT ((\(StackT f,x) -> (f,x)) ^>> app)

runStackT :: Arrow c => (StackWidening s a,s a) -> StackT s a c x y -> c x y
runStackT s (StackT f) = runConstT s f

runStackT' :: (Arrow c, Monoid (s a)) => StackWidening s a -> StackT s a c x y -> c x y
runStackT' s = runStackT (s,mempty)

stackT :: ((StackWidening s a,s a) -> c x y) -> StackT s a c x y
stackT f = StackT $ ConstT $ StaticT $ f

type instance Fix x y (StackT s () c) = StackT s x (Fix x y c)
instance (ArrowApply c, ArrowFix x y c) => ArrowFix x y (StackT s x c) where
  fix f = stackT $ \(stackWidening,stack) -> proc x -> do
    let (x',stack') = M.runState (stackWidening x) stack
    fix (runStackT (stackWidening,stack') . f . lift') -<< x'

