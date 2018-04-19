{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Transformer.Abstract.ReachingDefinitions where

import           Prelude hiding ((.),read)
import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Lift
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Fail
import           Control.Arrow.Store
import           Control.Arrow.Transformer.State

import           Data.Identifiable
import           Data.Order
import           Data.Abstract.Widening
import           Data.Hashable
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H

import           GHC.Generics
import           GHC.Exts

data ReachingDefs v l = ReachingDefs (HashSet (v,Maybe l)) | Top deriving (Eq,Generic)

empty :: ReachingDefs v l
empty = ReachingDefs H.empty

instance (Show v, Show l) => Show (ReachingDefs v l) where
  show (ReachingDefs vs) = show (H.toList vs)
  show Top = "⊤"

instance (Identifiable v, Identifiable l) => PreOrd (ReachingDefs v l) where
  _ ⊑ Top = True
  ReachingDefs xs ⊑ ReachingDefs ys = all (\x -> H.member x ys) xs
  _ ⊑ _ = False

instance (Identifiable v, Identifiable l) => Complete (ReachingDefs v l) where
  Top ⊔ _ = Top
  _ ⊔ Top = Top
  ReachingDefs xs ⊔ ReachingDefs ys = ReachingDefs (H.union xs ys)

instance (Identifiable v, Identifiable l) => Widening (ReachingDefs v l)

instance (Identifiable v, Identifiable l) => UpperBounded (ReachingDefs v l) where
  top = Top

instance (Hashable v, Hashable l) => Hashable (ReachingDefs v l)

instance (Identifiable v, Identifiable l) => IsList (ReachingDefs v l) where
  type Item (ReachingDefs v l) = (v,Maybe l)
  fromList = ReachingDefs . H.fromList
  toList (ReachingDefs vs) = H.toList vs
  toList Top = error "toList ⊤"

newtype ReachingDefinitions v l c x y = ReachingDefinitions (State (ReachingDefs v l) c x y)

runReachingDefinitions :: ReachingDefinitions v l c x y -> c (ReachingDefs v l,x) (ReachingDefs v l,y)
runReachingDefinitions (ReachingDefinitions f) = runState f

instance (Identifiable var, Identifiable lab, ArrowStore var val lab c)
  => ArrowStore var val lab (ReachingDefinitions var lab c) where
  read = lift read 
  write = ReachingDefinitions $ State $ proc (ReachingDefs defs,(x,v,l)) -> do
    write -< (x,v,l)
    returnA -< (ReachingDefs (H.insert (x,Just l) (H.filter (\(y,_) -> x /= y) defs)),())

type instance Fix x y (ReachingDefinitions v l c) = ReachingDefinitions v l (Fix (ReachingDefs v l,x) (ReachingDefs v l,y) c)
instance (ArrowFix (ReachingDefs v l,x) (ReachingDefs v l,y) c) => ArrowFix x y (ReachingDefinitions v l c) where
  fixA f = ReachingDefinitions $ State $ fixA $ \g ->
    runReachingDefinitions $ f $ ReachingDefinitions $ State $
      (\(defs,x) -> ((defs,x),defs)) ^>> first g >>^ (\((_,y),defs) -> (defs,y))

instance ArrowLift (ReachingDefinitions v l) where
  lift f = ReachingDefinitions (lift f)

instance (ArrowApply c) => ArrowApply (ReachingDefinitions v l c) where
  app = ReachingDefinitions ((\(ReachingDefinitions f,x) -> (f,x)) ^>> app)

instance (ArrowState s c) => ArrowState s (ReachingDefinitions v l c) where
  getA = lift getA
  putA = lift putA

deriving instance (Arrow c) => Category (ReachingDefinitions v l c)
deriving instance (Arrow c) => Arrow (ReachingDefinitions v l c)
deriving instance (ArrowChoice c) => ArrowChoice (ReachingDefinitions v l c)
deriving instance (ArrowReader r c) => ArrowReader r (ReachingDefinitions v l c)
deriving instance (ArrowFail e c) => ArrowFail e (ReachingDefinitions v l c)
