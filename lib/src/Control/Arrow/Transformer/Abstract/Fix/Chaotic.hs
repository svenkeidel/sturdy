{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Control.Arrow.Transformer.Abstract.Fix.Chaotic(ChaoticT,runChaoticT,iterateOuter,iterateInner) where

import           Prelude hiding (id,pred,lookup,map,head,iterate,(.),elem)

import           Control.Category
import           Control.Arrow hiding (loop)
import           Control.Arrow.Fix
import           Control.Arrow.Fix.Chaotic
import           Control.Arrow.Fix.Reuse
import           Control.Arrow.Fix.Cache as Cache
import           Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.Fix.Context as Context
import           Control.Arrow.State
import           Control.Arrow.Trans
import           Control.Arrow.Order(ArrowComplete(..),ArrowJoin(..),ArrowEffectCommutative)
import           Control.Arrow.Utils

import           Control.Arrow.Transformer.Writer

import           Data.Abstract.Stable

import           Data.Order
import           Data.Profunctor
import qualified Data.HashSet as H
import           Data.Identifiable
import           Data.Coerce

-- | Iterate on the innermost fixpoint component.
iterateInner :: (Identifiable a, LowerBounded b, ArrowStack a c, ArrowIterate a c, ArrowComponent a c, ArrowCache a b c, ArrowChoice c) => IterationStrategy c a b
{-# INLINE iterateInner #-}
iterateInner = detectLoop . go
  where
    go f = withComponent f $ proc (a,b,component) ->
      -- The call did not depend on any unstable calls. This means
      -- we are done and don't need to iterate.
      if H.null (head component)
      then do
        Cache.write -< (a,b,Stable)
        returnA -< b
      else do
        (stable,bNew) <- Cache.update -< (a,b)
        case stable of
          Stable   -> setComponent -< (component { head = H.delete a (head component) },bNew)
          Unstable -> go f -< a

-- | Iterate on the outermost fixpoint component.
iterateOuter :: (Identifiable a, LowerBounded b, ArrowStack a c, ArrowIterate a c, ArrowComponent a c, ArrowCache a b c, ArrowChoice c) => IterationStrategy c a b
{-# INLINE iterateOuter #-}
iterateOuter = detectLoop . go
  where
    go f = withComponent f $ proc (a,b,component) -> case () of
      -- The call did not depend on any unstable calls. This means
      -- we are done and don't need to iterate.
      () | H.null (head component) -> do
           Cache.write -< (a,b,Stable)
           setComponent -< (mempty,b)

      -- We are at the head of a fixpoint component. This means, we
      -- have to iterate until the head stabilized.
         | head component == H.singleton a -> do
           (stable,bNew) <- Cache.update -< (a,b)

           case stable of
             -- If the head of a fixpoint component is stable, set
             -- all elements in the body of the component as stable
             -- too and return.
             Stable -> do
               map Cache.setStable -< H.toList $ H.map (Stable,) (body component)
               setComponent -< (mempty, bNew)

             -- If the head of a fixpoint component is not stable, keep iterating.
             Unstable ->
               go f -< a

      -- We are inside an  fixpoint component, but its head has not stabilized.
         | otherwise -> do
           Cache.write -< (a,b,Unstable)
           setComponent -< (Component { head = H.delete a (head component),
                                        body = H.insert a (body component) }, b)

detectLoop :: (LowerBounded b, ArrowStack a c, ArrowCache a b c, ArrowIterate a c, ArrowChoice c) => IterationStrategy c a b
detectLoop f = proc a -> do
  loop <- Stack.elem -< a
  if loop
  then do
    m <- Cache.lookup -< a
    case m of
      Just (Stable,b) -> returnA -< b
      Just (Unstable,b) -> iterate -< (a, b)
      Nothing -> iterate -< (a, bottom)
  else Stack.push f -< a
{-# INLINE detectLoop #-}

newtype ChaoticT a c x y = ChaoticT (WriterT (Component a) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowStack a,ArrowCache a b,ArrowReuse a b,ArrowState s,ArrowContext ctx)

instance (Identifiable a, Arrow c, Profunctor c) => ArrowIterate a (ChaoticT a c) where
  iterate = lift (arr (first singleton))
  {-# INLINE iterate #-}

instance (Identifiable a, Arrow c, Profunctor c) => ArrowComponent a (ChaoticT a c) where
  setComponent = lift id
  withComponent f g = lift $ proc a -> do
    (component,b) <- unlift f -< a
    unlift g -< (a,b,component)
  {-# INLINE setComponent #-}
  {-# INLINE withComponent #-}

runChaoticT :: Profunctor c => ChaoticT a c x y -> c x y
runChaoticT (ChaoticT f) = rmap snd (runWriterT f)
{-# INLINE runChaoticT #-}

instance (Identifiable a, ArrowRun c) => ArrowRun (ChaoticT a c) where
  type Run (ChaoticT a c) x y = Run c x y
  run f = run (runChaoticT f)
  {-# INLINE run #-}

instance ArrowTrans (ChaoticT a c) where
  type Underlying (ChaoticT a c) x y = c x (Component a,y)

instance (Identifiable a, Profunctor c,ArrowApply c) => ArrowApply (ChaoticT a c) where
  app = ChaoticT (lmap (first coerce) app)
  {-# INLINE app #-}

deriving instance (Identifiable a, ArrowJoin c) => ArrowJoin (ChaoticT a c)
deriving instance (Identifiable a, ArrowComplete (Component a,y) c) => ArrowComplete y (ChaoticT a c)
instance (Identifiable a, ArrowEffectCommutative c) => ArrowEffectCommutative (ChaoticT a c)
