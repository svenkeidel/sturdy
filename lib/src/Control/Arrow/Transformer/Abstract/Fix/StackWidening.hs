{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.StackWidening where

import Prelude hiding (pred,lookup,map,head,iterate,(.))

import Control.Category
import Control.Arrow
import Control.Arrow.Cache
import Control.Arrow.Const
import Control.Arrow.Reader
import Control.Arrow.Trans
import Control.Arrow.Order(ArrowJoin(..),ArrowComplete(..),ArrowEffectCommutative)

import Control.Arrow.Transformer.Const
import Control.Arrow.Transformer.Reader

import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce
import Data.Empty
import Data.Order

import Data.Abstract.StackWidening
import Data.Abstract.Widening(Stable(..))
import Data.Maybe(fromMaybe)
import Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M

newtype StackT stack a c x y = StackT (ConstT (StackWidening stack a) (ReaderT (stack a) c) x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice)

runStackT :: (IsEmpty (stack a), Profunctor c)
          => StackWidening stack a -> StackT stack a c x y -> c x y
runStackT stackWiden (StackT f) = lmap (\x -> (empty,x)) (runReaderT (runConstT stackWiden f))
{-# INLINE runStackT #-}

newtype Cache a b = Cache (HashMap a (Stable,b))
instance (Show a, Show b) => Show (Cache a b) where
  show (Cache m) = show (M.toList m)

instance IsEmpty (Cache a b) where
  empty = Cache M.empty
  {-# INLINE empty #-}

instance (LowerBounded b, ArrowChoice c, ArrowCache a b c) => ArrowReuse a b (StackT stack a c) where
  reuse (StackT f) = StackT $ askConst $ \widen -> proc a -> do
    m <- lookup' -< a
    case m of
      Just (Stable,b) -> f -< (a,Cached (Stable,b))
      _ -> do
        stack <- ask -< ()
        let ((a',l),stack') = widen a stack
        m' <- lookup' -< a'
        case m' of
          Just (Stable,b) -> f -< (a',Cached (Stable,b))
          b -> case l of
                 NoLoop -> local f -< (stack',(a',Compute))
                 Loop -> f -< (a',Cached (fromMaybe (Instable,bottom) b))
    where lookup' = lift' (lift' lookup)

instance (ArrowCache a b c, ArrowChoice c, Profunctor c) => ArrowCache a b (StackT stack a c) where
  lookup = lift' lookup
  write = lift' write
  update = lift' update
  setStable = lift' setStable
  {-# INLINE lookup #-}
  {-# INLINE write #-}
  {-# INLINE update #-}
  {-# INLINE setStable #-}

instance ArrowLift (StackT stack a) where
  lift' f = StackT (lift' (lift' f))
  {-# INLINE lift' #-}

instance (IsEmpty (stack a), ArrowRun c) => ArrowRun (StackT stack a c) where
  type Run (StackT stack a c) x y = StackWidening stack a -> Run c x y
  run f stackWiden = run (runStackT stackWiden f)
  {-# INLINE run #-}

instance (Profunctor c,ArrowApply c) => ArrowApply (StackT cache a c) where
  app = StackT (app .# first coerce)
  {-# INLINE app #-}

instance (Complete y, ArrowEffectCommutative c) => ArrowComplete y (StackT stack a c) where
  StackT f <⊔> StackT g = StackT $ rmap (uncurry (⊔)) (f &&& g)
  {-# INLINE (<⊔>) #-}

instance (Arrow c, Profunctor c) => ArrowJoin (StackT stack a c) where
  joinSecond (StackT f) = StackT (second f)
  {-# INLINE joinSecond #-}

instance ArrowEffectCommutative c => ArrowEffectCommutative (StackT stack a c)
