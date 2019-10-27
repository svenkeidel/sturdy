{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-incomplete-patterns #-}
module Data.Abstract.Except where

import Prelude hiding (id,(.))

import Control.Arrow hiding (ArrowMonad)
import Control.Arrow.Monad
import qualified Control.Arrow.Order as O

import Control.Monad
import Control.DeepSeq

import Data.Profunctor
import Data.Bifunctor (Bifunctor(bimap))
import Data.Hashable
import Data.Order hiding (lub)
import Data.Traversable

import Data.Abstract.FreeCompletion (FreeCompletion(..))
import Data.Abstract.Widening
import Data.Abstract.Stable

import GHC.Generics (Generic, Generic1)
import GHC.TypeLits

-- | Abstrat domain for exceptions. This abstract domain approximates
-- error more precisely because 'Success ⋢ Fail'. Use this type for
-- analysis in languages that can handle exceptions.
data Except e x
  = Success x
  | Fail e
  | SuccessOrFail e x
  deriving (Eq, Generic, Generic1, NFData, NFData1)

instance (Show x, Show e) => Show (Except e x) where
  show (Success x) = show x
  show (Fail e) = "Fail " ++ show e
  show (SuccessOrFail e x) = "Success " ++ show x ++ " ⊔ Fail " ++ show e

instance (O.ArrowJoin c, ArrowChoice c, Profunctor c) => ArrowFunctor (Except e) c where
  mapA f =
    lmap toEither (arr Fail ||| rmap Success f ||| O.joinSecond (\(Fail e) (Success y) -> SuccessOrFail e y) (\(e,_) -> Fail e) (proc (_,x) -> rmap Success f -< x))
  {-# INLINABLE mapA #-}

instance (Complete e, O.ArrowJoin c, ArrowChoice c, Profunctor c) => ArrowMonad (Except e) c where
  mapJoinA f = lmap toEither (arr Fail ||| f ||| O.joinSecond lub (\(e,_) -> Fail e) (proc (_,x) -> f -< x))
    where
      lub (Fail e) m = case m of
        Fail e' -> Fail (e ⊔ e')
        Success y -> SuccessOrFail e y
        SuccessOrFail e' y -> SuccessOrFail (e ⊔ e') y
      {-# INLINE lub #-}
  {-# INLINABLE mapJoinA #-}

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
    (Success x, Success y) -> second Success (x `wa` y)
    (Success x, Fail e) -> (Unstable,SuccessOrFail e x)
    (Fail e, Success y) -> (Unstable,SuccessOrFail e y)
    (Fail e, Fail e') -> second Fail (e `we` e')
    (SuccessOrFail e x, Success y) -> (Unstable,SuccessOrFail e (snd (x `wa` y)))
    (Success x, SuccessOrFail e y) -> (Unstable,SuccessOrFail e (snd (x `wa` y)))
    (SuccessOrFail e x, Fail e') -> (Unstable,SuccessOrFail (snd (e `we` e')) x)
    (Fail e, SuccessOrFail e' y) -> (Unstable,SuccessOrFail (snd (e `we` e')) y)
    (SuccessOrFail e x, SuccessOrFail e' y) ->
       let (s,e'') = e `we` e'
           (s',z)  = x `wa` y
       in (s ⊔ s',SuccessOrFail e'' z)
{-# NOINLINE widening #-}

instance (PreOrd e, PreOrd a, Complete (FreeCompletion e), Complete (FreeCompletion a)) => Complete (FreeCompletion (Except e a)) where
  Lower m1 ⊔ Lower m2 = case bimap Lower Lower m1 ⊔ bimap Lower Lower m2 of
    Fail (Lower e) -> Lower (Fail e)
    Success (Lower a) -> Lower (Success a)
    SuccessOrFail (Lower e) (Lower a) -> Lower (SuccessOrFail e a)
    _ -> Top
  _ ⊔ _ = Top

instance (UpperBounded e, UpperBounded a) => UpperBounded (Except e a) where
  top = SuccessOrFail top top

instance (TypeError ('Text "Except does not have a lower bound. You probably want to use bottom of Data.Abstract.Terminating"), PreOrd a, PreOrd e) => LowerBounded (Except e a) where
  bottom = error "do not implement"

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
  {-# INLINE fmap #-}

instance Complete e => Applicative (Except e) where
  pure = return
  (<*>) = ap
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Complete e => Monad (Except e) where
  return = Success
  x >>= k = case x of
    Success y -> k y
    Fail e -> Fail e
    SuccessOrFail e y -> case k y of
      Success z -> SuccessOrFail e z
      Fail e' -> Fail (e ⊔ e')
      SuccessOrFail e' z -> SuccessOrFail (e ⊔ e') z
  {-# INLINE return #-}
  {-# INLINE (>>=) #-}

instance Foldable (Except e) where
  foldMap = foldMapDefault

instance Traversable (Except e) where
  traverse f (Success x) = Success <$> f x
  traverse _ (Fail e) = pure (Fail e)
  traverse f (SuccessOrFail e x) = SuccessOrFail e <$> f x

toEither :: Except e a -> Either e (Either a (e,a))
toEither (Fail e) = Left e
toEither (Success a) = Right (Left a)
toEither (SuccessOrFail e a) = Right (Right (e,a))
{-# INLINE toEither #-}

fromMaybe :: Maybe a -> Except () a
fromMaybe m = case m of
  Just a -> Success a
  Nothing -> Fail ()

toMaybe :: Except e a -> Maybe a
toMaybe (Success x) = Just x
toMaybe (Fail _) = Nothing
toMaybe (SuccessOrFail _ x) = Just x
