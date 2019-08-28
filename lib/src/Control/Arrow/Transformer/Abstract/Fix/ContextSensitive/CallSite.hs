{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.ContextSensitive.CallSite where

import Prelude hiding (lookup)

import Control.Category
import Control.Arrow
import Control.Arrow.Cache
import Control.Arrow.Trans
import Control.Arrow.Order

import Control.Arrow.Transformer.Reader

import Data.Profunctor.Unsafe
import Data.Coerce
import Data.Empty
import Data.Identifiable
import Data.Abstract.CallString
import Data.Abstract.Context
import Data.Monoidal
import GHC.TypeLits

newtype CallSiteT k lab c x y = CallSiteT (ReaderT (CallString k lab) c x y)
  deriving (Category,Arrow,ArrowChoice,Profunctor)

instance (Identifiable lab, KnownNat k, ArrowRecurse (CallString k lab,(lab,a)) b c) => ArrowRecurse (lab,a) b (CallSiteT k lab c) where
  -- | Pushes the label on the call string and truncate the call string to 'k'.
  recurse f = lift $ proc (callString,(lab,a)) -> do
    recurse (lmap assoc2 (unlift f)) -< (push lab callString,(lab,a))
  {-# INLINE recurse #-}

instance ArrowCache (CallString k lab,(lab,a)) b c => ArrowCache (lab,a) b (CallSiteT k lab c) where
  lookup = lift $ lookup
  write = lift $ lmap (\(cont,((l,a),b,s)) -> ((cont,(l,a)),b,s)) write
  update = lift $ lmap (\(cont,((l,a),b)) -> ((cont,(l,a)),b)) update
  setStable = lift $ lmap (\(cont,(s,(l,a))) -> (s,(cont,(l,a)))) setStable
  {-# INLINE lookup #-}
  {-# INLINE write #-}
  {-# INLINE update #-}
  {-# INLINE setStable #-}

runCallSiteT :: (Profunctor c) => CallSiteT k lab c x y -> c x y
runCallSiteT (CallSiteT f) = lmap (\x -> (empty,x)) (runReaderT f)
{-# INLINE runCallSiteT #-}

instance (ArrowRun c) => ArrowRun (CallSiteT k lab c) where
  type Run (CallSiteT k lab c) x y = Run c x y
  run f = run (runCallSiteT f)
  {-# INLINE run #-}

instance ArrowTrans (CallSiteT k lab c) where
  type Underlying (CallSiteT k lab c) x y = c (CallString k lab,x) y

instance ArrowEffectCommutative c => ArrowEffectCommutative (CallSiteT k lab c)

instance (Profunctor c,ArrowApply c) => ArrowApply (CallSiteT k lab c) where
  app = CallSiteT (app .# first coerce)
  {-# INLINE app #-}
