{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
module Data.Abstract.Except where

import Control.Arrow as A
import Control.Monad
import Control.DeepSeq

import Data.Abstract.FreeCompletion (FreeCompletion(..))
import Data.Abstract.Widening
import Data.Bifunctor
import Data.Hashable
import Data.Order
import Data.Traversable

import GHC.Generics (Generic, Generic1)

-- | Abstrat domain for exceptions. This abstract domain approximates
-- error more precisely because 'Success ⋢ Fail'. Use this type for
-- analysis in languages that can handle exceptions.
data Except e x
  = Success x
  | Fail e
  | SuccessOrFail e x
  deriving (Eq, Show, Generic, Generic1, NFData, NFData1)

instance (Hashable e, Hashable x) => Hashable (Except e x) where
  hashWithSalt s (Success x) = s `hashWithSalt` (0::Int) `hashWithSalt` x
  hashWithSalt s (Fail e) = s `hashWithSalt` (1::Int) `hashWithSalt` e
  hashWithSalt s (SuccessOrFail e x) = s `hashWithSalt` (2 ::Int) `hashWithSalt` e `hashWithSalt` x

instance (PreOrd e, PreOrd a) => PreOrd (Except e a) where
  m1 ⊑ m2 = case (m1,m2) of
    (Fail e, Fail e') ->  e ⊑ e'
    (Success a, Success b) -> a ⊑ b
    (Success a, SuccessOrFail _ b) -> a ⊑ b
    (SuccessOrFail e a, SuccessOrFail e' b) -> e ⊑ e' && a ⊑ b
    (Fail e, SuccessOrFail e' _) -> e ⊑ e'
    (_, _) -> False

instance (Complete e, Complete a) => Complete (Except e a) where
  (⊔) = toJoin2 widening (⊔) (⊔)

widening :: Widening e -> Widening a -> Widening (Except e a)
widening we wa m1 m2 = case (m1,m2) of
    (Success x, Success y) -> A.second Success (x `wa` y)
    (Success x, Fail e) -> (Instable,SuccessOrFail e x)
    (Fail e, Success y) -> (Instable,SuccessOrFail e y)
    (Fail e, Fail e') -> A.second Fail (e `we` e')
    (SuccessOrFail e x, Success y) -> (Instable,SuccessOrFail e (snd (x `wa` y)))
    (Success x, SuccessOrFail e y) -> (Instable,SuccessOrFail e (snd (x `wa` y)))
    (SuccessOrFail e x, Fail e') -> (Instable,SuccessOrFail (snd (e `we` e')) x)
    (Fail e, SuccessOrFail e' y) -> (Instable,SuccessOrFail (snd (e `we` e')) y)
    (SuccessOrFail e x, SuccessOrFail e' y) ->
       let (s,e'') = e `we` e'
           (s',z)  = x `wa` y
       in (s ⊔ s',SuccessOrFail e'' z)

instance (PreOrd e, PreOrd a, Complete (FreeCompletion e), Complete (FreeCompletion a)) => Complete (FreeCompletion (Except e a)) where
  Lower m1 ⊔ Lower m2 = case (bimap Lower Lower m1 ⊔ bimap Lower Lower m2) of
    Fail (Lower e) -> Lower (Fail e)
    Success (Lower a) -> Lower (Success a)
    SuccessOrFail (Lower e) (Lower a) -> Lower (SuccessOrFail e a)
    _ -> Top
  _ ⊔ _ = Top

instance (UpperBounded e, UpperBounded a) => UpperBounded (Except e a) where
  top = SuccessOrFail top top

-- instance (LowerBounded e, LowerBounded a) => LowerBounded (Except e a) where
--   bottom = SuccessOrFail bottom bottom

instance (PreOrd a, PreOrd e, UpperBounded (FreeCompletion e), UpperBounded (FreeCompletion a))
  => UpperBounded (FreeCompletion (Except e a)) where
  top = case (top,top) of
    (Lower e,Lower a) -> Lower (SuccessOrFail e a)
    (_,_) -> Top

instance Bifunctor Except where
  bimap f g x = case x of
    Fail e -> Fail (f e)
    Success a -> Success (g a)
    SuccessOrFail e a -> SuccessOrFail (f e) (g a)

instance Functor (Except e) where
  fmap f r = case r of
    Success a -> Success (f a)
    Fail e -> Fail e
    SuccessOrFail e a -> SuccessOrFail e (f a)

instance Complete e => Applicative (Except e) where
  pure = return
  (<*>) = ap

instance Complete e => Monad (Except e) where
  return = arr Success
  x >>= k = case x of
    Success y -> k y
    Fail e -> Fail e
    SuccessOrFail e y -> case k y of
      Success z -> SuccessOrFail e z
      Fail e' -> Fail (e ⊔ e')
      SuccessOrFail e' z -> SuccessOrFail (e ⊔ e') z

instance PreOrd a => LowerBounded (Except () a) where
  bottom = Fail ()

instance Foldable (Except e) where
  foldMap = foldMapDefault

instance Traversable (Except e) where
  traverse f (Success x) = Success <$> f x
  traverse _ (Fail e) = pure (Fail e)
  traverse f (SuccessOrFail e x) = SuccessOrFail e <$> f x

fromMaybe :: Maybe a -> Except () a
fromMaybe m = case m of
  Just a -> Success a
  Nothing -> Fail ()

toMaybe :: Except e a -> Maybe a
toMaybe (Success x) = Just x
toMaybe (Fail _) = Nothing
toMaybe (SuccessOrFail _ x) = Just x
