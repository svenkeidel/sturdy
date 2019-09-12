{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
module Data.Abstract.Either where

import Prelude hiding (Either(..))

import Control.Monad(ap)

import Data.Abstract.Stable
import Data.Abstract.Widening
import Data.Bifunctor
import Data.Hashable
import Data.Order
import Data.Traversable

-- | Abstract 'Either' type with an upper bound for 'Left' and 'Right'.
data Either a b = Left a | Right b | LeftRight a b
  deriving (Eq,Ord,Show)

instance (Hashable a, Hashable b) => Hashable (Either a b) where
  hashWithSalt s (Left a) = s `hashWithSalt` (1::Int) `hashWithSalt` a
  hashWithSalt s (Right b) = s `hashWithSalt` (2::Int) `hashWithSalt` b
  hashWithSalt s (LeftRight a b) = s `hashWithSalt` (3 ::Int) `hashWithSalt` a `hashWithSalt` b
  {-# INLINE hashWithSalt #-}

instance (PreOrd a, PreOrd b) => PreOrd (Either a b) where
  m1 ⊑ m2 = case (m1,m2) of
    (Left a, Left a') ->  a ⊑ a'
    (Right b, Right b') -> b ⊑ b'
    (LeftRight a b, LeftRight a' b') -> a ⊑ a' && b ⊑ b'
    (Right b, LeftRight _ b') -> b ⊑ b'
    (Left a, LeftRight a' _) -> a ⊑ a'
    (_, _) -> False
  {-# INLINE (⊑) #-}

instance (Complete a, Complete b) => Complete (Either a b) where
  m1 ⊔ m2 = case (m1,m2) of
    (Right b, Right b') -> Right (b ⊔ b')
    (Right b, Left a') -> LeftRight a' b
    (Left a, Right b') -> LeftRight a b'
    (Left a, Left a') -> Left (a ⊔ a')
    (LeftRight a b, Right b') -> LeftRight a (b ⊔ b')
    (Right b, LeftRight a' b') -> LeftRight a' (b ⊔ b')
    (LeftRight a b, Left a') -> LeftRight (a ⊔ a') b
    (Left a, LeftRight a' b') -> LeftRight (a ⊔ a') b'
    (LeftRight a b, LeftRight a' b') -> LeftRight (a ⊔ a') (b ⊔ b')
  {-# INLINE (⊔) #-}

widening :: Widening a -> Widening b -> Widening (Either a b)
widening wa wb m1 m2 = case (m1,m2) of
    (Right b, Right b') -> second Right (b `wb` b')
    (Right b, Left a') -> (Unstable,LeftRight a' b)
    (Left a, Right b') -> (Unstable,LeftRight a b')
    (Left a, Left a') -> second Left (a `wa` a')
    (LeftRight a b, Right b') -> let (_,b'') = b `wb` b' in (Unstable,LeftRight a b'') 
    (Right b, LeftRight a' b') -> let (_,b'') = b `wb` b' in (Unstable,LeftRight a' b'')
    (LeftRight a b, Left a') -> let (_,a'') = a `wa` a' in (Unstable,LeftRight a'' b)
    (Left a, LeftRight a' b') -> let (_,a'') = a `wa` a' in (Unstable,LeftRight a'' b')
    (LeftRight a b, LeftRight a' b') -> let (sa,a'') = a `wa` a'
                                            (sb,b'') = b `wb` b'
                                        in (sa ⊔ sb,LeftRight a'' b'')


instance Bifunctor Either where
  bimap f g x = case x of
    Left a -> Left (f a)
    Right b -> Right (g b)
    LeftRight a b -> LeftRight (f a) (g b)

instance Functor (Either a) where
  fmap f r = case r of
    Left a -> Left a
    Right b -> Right (f b)
    LeftRight a b -> LeftRight a (f b)

instance Complete a => Applicative (Either a) where
  pure = return
  (<*>) = ap

instance Complete a => Monad (Either a) where
  return = Right
  x >>= k = case x of
    Left a -> Left a
    Right b -> k b
    LeftRight a b -> case k b of
      Left a' -> Left (a ⊔ a')
      Right b' -> LeftRight a b'
      LeftRight a' b' -> LeftRight (a ⊔ a') b'

instance Foldable (Either a) where
  foldMap = foldMapDefault

instance Traversable (Either a) where
  traverse _ (Left a) = pure (Left a)
  traverse f (Right b) = Right <$> f b
  traverse f (LeftRight a b) = LeftRight a <$> f b
