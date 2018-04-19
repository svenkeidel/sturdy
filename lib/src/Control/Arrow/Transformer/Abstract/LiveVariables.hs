{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
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
import qualified Data.HashSet as H
import           Data.Identifiable
import           Data.Order
import           Data.Abstract.Widening

import           GHC.Exts
import           GHC.Generics
import           Text.Printf

data LiveVars v = LiveVars (HashSet v) | Top deriving (Eq,Generic)


instance Show v => Show (LiveVars v) where
  show (LiveVars vs) = show (H.toList vs)
  show Top = "⊤"

instance Identifiable v => PreOrd (LiveVars v) where
  _ ⊑ Top = True
  LiveVars xs ⊑ LiveVars ys = all (\x -> H.member x ys) xs
  _ ⊑ _ = False

instance Identifiable v => Complete (LiveVars v) where
  Top ⊔ _ = Top
  _ ⊔ Top = Top
  LiveVars xs ⊔ LiveVars ys = LiveVars (H.union xs ys)

instance Identifiable v => Widening (LiveVars v)

instance Identifiable v => UpperBounded (LiveVars v) where
  top = Top

instance Identifiable v => IsList (LiveVars v) where
  type Item (LiveVars v) = v
  fromList = LiveVars . H.fromList
  toList (LiveVars vs) = H.toList vs
  toList Top = error "toList ⊤"

instance Identifiable v => Monoid (LiveVars v) where
  mempty = LiveVars mempty
  mappend Top _ = Top
  mappend _ Top = Top
  mappend (LiveVars xs) (LiveVars ys) = LiveVars (xs `mappend` ys)

newtype LiveVarsTrans v = LiveVarsTrans (LiveVars v -> LiveVars v,LiveVars v -> LiveVars v)

entry :: Identifiable v => LiveVarsTrans v -> LiveVars v
entry (LiveVarsTrans (en,_)) = en mempty

exit :: Identifiable v => LiveVarsTrans v -> LiveVars v
exit (LiveVarsTrans (_,ex)) = ex mempty

instance (Identifiable v, Show v) => Show (LiveVarsTrans v) where
  show vs = printf "{entry = %s, exit = %s}" (show (entry vs)) (show (exit vs))

instance Identifiable v => Eq (LiveVarsTrans v) where
  l1 == l2 = entry l1 == entry l2 && exit l1 == exit l2

instance Identifiable v => PreOrd (LiveVarsTrans v) where
  l1 ⊑ l2  = entry l1 ⊑ entry l2 && exit l1 ⊑ exit l2

instance Identifiable v => Complete (LiveVarsTrans v) where
  LiveVarsTrans v1 ⊔ LiveVarsTrans v2 = LiveVarsTrans (v1 ⊔ v2)

instance Identifiable v => Widening (LiveVarsTrans v)

instance Identifiable v => UpperBounded (LiveVarsTrans v) where
  top = LiveVarsTrans (top,top)

instance Monoid (LiveVarsTrans v) where
  mempty = LiveVarsTrans (id,id)
  mappend (LiveVarsTrans (en1,_)) (LiveVarsTrans (en2,ex2)) = LiveVarsTrans (en2 >>> en1, ex2)

live :: Identifiable v => v -> LiveVarsTrans v
live x = let en (LiveVars vars) = LiveVars (H.insert x vars) 
             en Top = Top
         in LiveVarsTrans (en,id)

dead :: Identifiable v => v -> LiveVarsTrans v
dead x = let en (LiveVars vars) = LiveVars (H.delete x vars) 
             en Top = Top
         in LiveVarsTrans (en,id)

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

newBasicBlock :: Arrow c => c x (LiveVarsTrans v,y) -> c x (LiveVarsTrans v,y)
newBasicBlock f = f >>^ first (\(LiveVarsTrans (en,_)) -> LiveVarsTrans (en,en))

type instance Fix x y (LiveVariables v c) = LiveVariables v (Fix x (LiveVarsTrans v,y) c)
instance (ArrowFix x (LiveVarsTrans v,y) c) => ArrowFix x y (LiveVariables v c) where
  fixA f = LiveVariables (Writer (fixA (runLiveVariables . f . LiveVariables . Writer. newBasicBlock)))

instance ArrowLift (LiveVariables v) where
  lift f = LiveVariables (lift f)

instance (ArrowApply c) => ArrowApply (LiveVariables v c) where
  app = LiveVariables ((\(LiveVariables f,x) -> (f,x)) ^>> app)

deriving instance (Arrow c) => Category (LiveVariables v c)
deriving instance (Arrow c) => Arrow (LiveVariables v c)
deriving instance (ArrowChoice c) => ArrowChoice (LiveVariables v c)
deriving instance (ArrowReader r c) => ArrowReader r (LiveVariables v c)
deriving instance (ArrowFail e c) => ArrowFail e (LiveVariables v c)
deriving instance (ArrowState s c) => ArrowState s (LiveVariables v c)

deriving instance PreOrd (c x (LiveVarsTrans v,y)) => PreOrd (LiveVariables v c x y)
deriving instance LowerBounded (c x (LiveVarsTrans v,y)) => LowerBounded (LiveVariables v c x y)
deriving instance Complete (c x (LiveVarsTrans v,y)) => Complete (LiveVariables v c x y)
deriving instance CoComplete (c x (LiveVarsTrans v,y)) => CoComplete (LiveVariables v c x y)
deriving instance UpperBounded (c x (LiveVarsTrans v,y)) => UpperBounded (LiveVariables v c x y)

