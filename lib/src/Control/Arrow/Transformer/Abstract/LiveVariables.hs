{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Abstract.LiveVariables where

import           Prelude hiding (id,(.),read)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Lift
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Store
import           Control.Arrow.Writer
import           Control.Arrow.Transformer.Writer

import           Data.HashSet (HashSet)
import           Data.Hashable
import qualified Data.HashSet as H
import           Data.Identifiable
import           Data.Order
import           Data.Abstract.Widening

import           GHC.Exts
import           Text.Printf

newtype LiveVars v = LiveVars (HashSet v) deriving (Eq,Hashable)

instance Show v => Show (LiveVars v) where
  show (LiveVars vs) = show (H.toList vs)

instance Identifiable v => PreOrd (LiveVars v) where
  LiveVars xs ⊑ LiveVars ys = all (\x -> H.member x ys) xs

instance Identifiable v => Complete (LiveVars v) where
  LiveVars xs ⊔ LiveVars ys = LiveVars (H.union xs ys)

instance Identifiable v => Widening (LiveVars v)

instance Identifiable v => IsList (LiveVars v) where
  type Item (LiveVars v) = v
  fromList = LiveVars . H.fromList
  toList (LiveVars vs) = H.toList vs

instance Identifiable v => Monoid (LiveVars v) where
  mempty = LiveVars mempty
  mappend (LiveVars xs) (LiveVars ys) = LiveVars (xs `mappend` ys)

newtype LiveVarsTrans v = LiveVarsTrans (LiveVars v -> LiveVars v,LiveVars v)

entry :: LiveVarsTrans v -> LiveVars v
entry (LiveVarsTrans (f,x)) = f x

exit :: LiveVarsTrans v -> LiveVars v
exit (LiveVarsTrans (_,x)) = x

instance Show v => Show (LiveVarsTrans v) where
  show vs = printf "{entry = %s, exit = %s}" (show (entry vs)) (show (exit vs))

instance Identifiable v => Eq (LiveVarsTrans v) where
  l1 == l2 = entry l1 == entry l2 && exit l1 == exit l2

instance Identifiable v => PreOrd (LiveVarsTrans v) where
  l1 ⊑ l2  = entry l1 ⊑ entry l2 && exit l1 ⊑ exit l2

instance Identifiable v => Complete (LiveVarsTrans v) where
  LiveVarsTrans v1 ⊔ LiveVarsTrans v2 = LiveVarsTrans (v1 ⊔ v2)

instance Identifiable v => Widening (LiveVarsTrans v)

instance Identifiable v => Monoid (LiveVarsTrans v) where
  mempty = LiveVarsTrans (id,mempty)
  mappend (LiveVarsTrans (f,x)) (LiveVarsTrans (g,y)) = LiveVarsTrans (f . g,mappend x y)

live :: Identifiable v => v -> LiveVarsTrans v
live x = LiveVarsTrans (\(LiveVars vars) -> LiveVars (H.insert x vars),mempty)

dead :: Identifiable v => v -> LiveVarsTrans v
dead x = LiveVarsTrans (\(LiveVars vars) -> LiveVars (H.delete x vars),mempty)

newtype LiveVariables v c x y = LiveVariables (Writer (LiveVarsTrans v) c x y)

runLiveVariables :: LiveVariables v c x y -> c x (LiveVarsTrans v,y)
runLiveVariables (LiveVariables f) = runWriter f

instance (Identifiable var, ArrowStore var val c) => ArrowStore var val (LiveVariables var c) where
  read = LiveVariables $ proc x -> do
    tellA -< live x
    read -< x
  write = LiveVariables $ proc (x,v) -> do
    tellA -< dead x
    write -< (x,v)

type instance Fix x y (LiveVariables v c) = LiveVariables v (Fix x (LiveVarsTrans v,y) c)
instance (Identifiable v, ArrowFix x (LiveVarsTrans v,y) c) => ArrowFix x y (LiveVariables v c) where
  fixA f = LiveVariables (Writer (fixA (runLiveVariables . f . commit . LiveVariables . Writer)))

commit :: Arrow c => LiveVariables v c x y -> LiveVariables v c x y
commit (LiveVariables (Writer f)) = LiveVariables $ Writer $
  f >>^ first (\(LiveVarsTrans (g,x)) -> (LiveVarsTrans (id,g x)))

instance Identifiable v => ArrowLift (LiveVariables v) where
  lift f = LiveVariables (lift f)

instance (Identifiable v, ArrowApply c) => ArrowApply (LiveVariables v c) where
  app = LiveVariables ((\(LiveVariables f,x) -> (f,x)) ^>> app)

deriving instance (Identifiable v, Arrow c) => Category (LiveVariables v c)
deriving instance (Identifiable v, Arrow c) => Arrow (LiveVariables v c)
deriving instance (Identifiable v, ArrowChoice c) => ArrowChoice (LiveVariables v c)
deriving instance (Identifiable v, ArrowReader r c) => ArrowReader r (LiveVariables v c)
deriving instance (Identifiable v, ArrowFail e c) => ArrowFail e (LiveVariables v c)
deriving instance (Identifiable v, ArrowState s c) => ArrowState s (LiveVariables v c)

deriving instance PreOrd (c x (LiveVarsTrans v,y)) => PreOrd (LiveVariables v c x y)
deriving instance LowerBounded (c x (LiveVarsTrans v,y)) => LowerBounded (LiveVariables v c x y)
deriving instance Complete (c x (LiveVarsTrans v,y)) => Complete (LiveVariables v c x y)
deriving instance CoComplete (c x (LiveVarsTrans v,y)) => CoComplete (LiveVariables v c x y)
deriving instance UpperBounded (c x (LiveVarsTrans v,y)) => UpperBounded (LiveVariables v c x y)

