{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.StackWidening where

import           Prelude hiding (pred,lookup,map,head,iterate,(.))

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Trans
import           Control.Arrow.Order(ArrowJoin(..),ArrowComplete(..))
import           Control.Arrow.Transformer.Reader

import           Data.Profunctor
import           Data.Profunctor.Unsafe((.#))
import           Data.Coerce
import           Data.Empty

import           Data.Abstract.StackWidening(StackWidening)

newtype StackWideningT stack a c x y = StackWideningT (ReaderT (stack a) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowComplete z,ArrowJoin,ArrowTrans)

runStackWideningT :: (IsEmpty (stack a), Profunctor c) => StackWideningT stack a c x y -> c x y
runStackWideningT (StackWideningT f) = lmap (\x -> (empty,x)) (runReaderT f)

stackWidening :: (Profunctor c, ArrowApply c) => StackWidening stack a -> IterationStrategy c a b -> IterationStrategy (StackWideningT stack a c) a b
stackWidening stackWiden strat f = lift $ proc (stack,a) -> do
  let (a',stack') = stackWiden a stack
  strat (proc x -> (unlift f) -< (stack',x)) -<< a'

instance (IsEmpty (stack a), ArrowRun c) => ArrowRun (StackWideningT stack a c) where
  type Rep (StackWideningT stack a c) x y = Rep c x y
  run = run . runStackWideningT

instance (Profunctor c,ArrowApply c) => ArrowApply (StackWideningT stack a c) where
  app = StackWideningT (app .# first coerce)
