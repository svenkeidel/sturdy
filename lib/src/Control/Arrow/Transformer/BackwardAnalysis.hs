{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Transformer.BackwardAnalysis where

import Prelude hiding (map)
import Data.Identifiable
import Data.HashSet (HashSet)
import qualified Data.HashSet as H
import Control.Arrow
import Control.Arrow.Effect
import Control.Arrow.Transformer.Effect

data Backward a = Backward (HashSet a,HashSet a -> HashSet a) | Top
type BackwardAnalysis e = Effect (Backward e)

map :: (HashSet a -> HashSet a) -> Backward a -> Backward a
map f (Backward (s,g)) = Backward (s,f >>> g)
map _ Top = Top

instance (Identifiable e,Arrow c) => ArrowEffect (HashSet e) (Effect (Backward e) c) where
  record f = modifyEntrySet $ \(en,x) -> map (f x) en

instance Identifiable a => Monoid (Backward a) where
  mempty = Backward (H.empty,id)
  mappend (Backward (s,_)) (Backward (s',g)) = Backward (H.union s s',g)
  mappend Top _ = Top
  mappend _ Top = Top
