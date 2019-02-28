{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Abstract.FreeCompletion where

import Control.Arrow hiding (ArrowMonad)
import Control.Arrow.Monad
import Control.Applicative
import Control.Monad
import Control.DeepSeq

import Data.Profunctor
import Data.Abstract.Widening
import Data.Hashable
import Data.Order
import Data.Text(Text)

import GHC.Generics(Generic)
import GHC.Exts

data FreeCompletion a = Lower a | Top deriving (Eq,Functor,Traversable,Foldable,Generic)

instance Show a => Show (FreeCompletion a) where
  show Top = "⊤"
  show (Lower a) = show a

instance NFData a => NFData (FreeCompletion a)

instance Hashable a => Hashable (FreeCompletion a) where
  hashWithSalt s (Lower a) = s `hashWithSalt` a
  hashWithSalt s Top = s `hashWithSalt` (2::Int)

instance Applicative FreeCompletion where
  pure = return
  (<*>) = ap

instance Monad FreeCompletion where
  return = Lower
  Lower x >>= k = k x
  Top >>= _ = Top

instance (ArrowChoice c, Profunctor c) => ArrowFunctor FreeCompletion c c where
  mapA f = lmap toEither (arr (const Top) ||| rmap Lower f)

instance (ArrowChoice c, Profunctor c) => ArrowMonad FreeCompletion c where
  mapJoinA f = lmap toEither (arr (const Top) ||| f)

instance PreOrd a => PreOrd (FreeCompletion a) where
  _ ⊑ Top = True
  Top ⊑ _ = False
  Lower a ⊑ Lower b = a ⊑ b

  Top ≈ Top = True
  Lower a ≈ Lower b = a ≈ b
  _ ≈ _ = False

instance (PreOrd a, Complete (FreeCompletion a),
          PreOrd b, Complete (FreeCompletion b)) => Complete (FreeCompletion (a,b)) where
  Lower (a1,b1) ⊔ Lower (a2,b2) = case (Lower a1 ⊔ Lower a2, Lower b1 ⊔ Lower b2) of
    (Lower a, Lower b) -> Lower (a,b)
    (_, _) -> Top
  _ ⊔ _ = Top

instance (PreOrd a, UpperBounded (FreeCompletion a),
          PreOrd b, UpperBounded (FreeCompletion b)) => UpperBounded (FreeCompletion (a,b)) where
  top = case (top,top) of
    (Lower a, Lower b) -> Lower (a,b)
    (_,_) -> Top

instance (PreOrd x, Complete (FreeCompletion x)) => Complete (FreeCompletion [x]) where
  Lower xs ⊔ Lower ys | eqLength xs ys = zipWithM (\x y -> Lower x ⊔ Lower y) xs ys
    where
      eqLength :: [a] -> [b] -> Bool
      eqLength [] [] = True
      eqLength (_:as) (_:bs) = eqLength as bs
      eqLength _ _ = False
  _ ⊔ _ = Top

instance Complete (FreeCompletion Int) where
  Lower x ⊔ Lower y | x == y = Lower x
  _ ⊔ _ = Top

instance Complete (FreeCompletion Text) where
  Lower x ⊔ Lower y | x == y = Lower x
  _ ⊔ _ = Top

instance CoComplete a => CoComplete (FreeCompletion a) where
  Lower a ⊓ Lower b = Lower (a ⊓ b) 
  x ⊓ Top = x
  Top ⊓ y = y

instance LowerBounded a => LowerBounded (FreeCompletion a) where
  bottom = Lower bottom

instance Num a => Num (FreeCompletion a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (FreeCompletion a) where
  (/) = liftA2 (/)
  fromRational = pure . fromRational

instance Complete (FreeCompletion ()) where
  Lower _ ⊔ Lower _ = Lower ()
  _ ⊔ _ = Top

instance IsString s => IsString (FreeCompletion s) where
  fromString = Lower . fromString

toEither :: FreeCompletion a -> Either () a
toEither Top = Left ()
toEither (Lower a) = Right a

widening :: Widening a -> Widening (FreeCompletion a)
widening wa (Lower a) (Lower a') = second Lower (a `wa` a')
widening _ Top Top = (Stable,Top)
widening _ (Lower _) Top = (Instable,Top)
widening _ Top (Lower _) = (Instable,Top)

fromCompletion :: a -> FreeCompletion a -> a
fromCompletion a Top = a
fromCompletion _ (Lower a) = a
