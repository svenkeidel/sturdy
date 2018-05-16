{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.ForwardAnalysis where

import           Prelude hiding (map)

import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.Order
import           Data.Identifiable
import           Data.Abstract.Widening
import           Data.Hashable

import           Control.Arrow
import           Control.Arrow.Effect
import           Control.Arrow.Transformer.Effect

import           GHC.Generics
import           GHC.Exts

data Forward a = Forward (HashSet a) | Top deriving (Generic,Eq)
type ForwardAnalysis e = Effect (Forward e)

runForwardAnalysis :: Arrow c => ForwardAnalysis e c x y -> c (Forward e,x) y
runForwardAnalysis f = runEffect f

runForwardAnalysis' :: Arrow c => ForwardAnalysis e c x y -> c (Forward e,x) (Forward e,(Forward e,y))
runForwardAnalysis' f = runEffect' f

map :: (HashSet a -> HashSet b) -> Forward a -> Forward b
map f (Forward xs) = Forward (f xs)
map _ Top = Top

instance (Show a) => Show (Forward a) where
  show (Forward f) = show (H.toList f)
  show Top = "⊤"

instance Identifiable a => PreOrd (Forward a) where
  _ ⊑ Top = True
  Forward xs ⊑ Forward ys = all (\x -> H.member x ys) xs
  _ ⊑ _ = False

instance Identifiable a => Complete (Forward a) where
  Top ⊔ _ = Top
  _ ⊔ Top = Top
  Forward xs ⊔ Forward ys = Forward (H.union xs ys)

instance Identifiable a => Widening (Forward a)

instance Identifiable a => UpperBounded (Forward a) where
  top = Top

instance Hashable a => Hashable (Forward a)

instance Identifiable a => IsList (Forward a) where
  type Item (Forward a) = a
  fromList = Forward . H.fromList
  toList (Forward vs) = H.toList vs
  toList Top = error "toList ⊤"

instance Arrow c => ArrowEffect (HashSet e) (Effect (Forward e) c) where
  record f = modifyEntrySet $ \(en,x) -> map (f x) en

instance Monoid (Forward e) where
  mempty = Forward H.empty
  mappend _ a = a
