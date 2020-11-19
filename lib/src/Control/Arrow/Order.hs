{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Order where

import           Prelude hiding ((.),IO)

import           Control.Category
import           Control.Comonad
import           Control.Arrow hiding (ArrowMonad)
import           Control.Arrow.Monad
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.Cokleisli
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Cont
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.IO
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.ST
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Static
import           Control.Arrow.Transformer.Writer

import           Data.Order(Complete(..))
import           Data.Profunctor

class (Arrow c, Profunctor c) => ArrowLowerBounded y c where
  bottom :: c x y

class (Arrow c, Profunctor c) => ArrowComplete y c where
  (<⊔>) :: c x y -> c x y -> c x y

instance Complete y => ArrowComplete y (->) where
  (<⊔>) f g x = f x ⊔ g x
  {-# INLINE (<⊔>) #-}

class (Arrow c, Profunctor c) => ArrowJoin c where
  joinSecond :: (y -> y -> y) -> (x -> y) -> c x y -> c x y

instance ArrowJoin (->) where
  joinSecond lub f g x = f x `lub` g x
  {-# INLINE joinSecond #-}

-- | Joins a list of arguments. Use it with idiom brackets:
-- @
--   let a = ...; b = ...; xs = ...
--   (| joinList (returnA -< a) (\x -> f -< x+b) |) xs
-- @
joinList :: (ArrowChoice c, ArrowComplete y c) => c (e,s) y -> c (e,(x,s)) y -> c (e,([x],s)) y
joinList empty f = proc (e,(l,s)) -> case l of
  []     -> empty -< (e,s)
  [x]    -> f -< (e,(x,s))
  (x:xs) -> (f -< (e,(x,s))) <⊔> (joinList empty f -< (e,(xs,s)))
{-# INLINABLE joinList #-}

joinList1 :: (ArrowChoice c, ArrowLowerBounded y c, ArrowComplete y c) => c (e,(x,s)) y -> c (e,([x],s)) y
joinList1 f = proc (e,(l,s)) -> case l of
  []     -> bottom -< ()
  [x]    -> f -< (e,(x,s))
  (x:xs) -> (f -< (e,(x,s))) <⊔> (joinList1 f -< (e,(xs,s)))
{-# INLINABLE joinList1 #-}

joinList1' :: (ArrowChoice c, ArrowLowerBounded y c, ArrowComplete y c) => c (x,e) y -> c ([x],e) y
joinList1' f = proc (l,e) -> case l of
  []     -> bottom -< ()
  [x]    -> f -< (x,e)
  (x:xs) -> (f -< (x,e)) <⊔> (joinList1' f -< (xs,e))
{-# INLINABLE joinList1' #-}

------------- Instances --------------
instance (ArrowComonad f c, ArrowComplete y c) => ArrowComplete y (CokleisliT f c) where
  f <⊔> g = lift $ unlift f <⊔> unlift g
  {-# INLINE (<⊔>) #-}

instance (ArrowComonad f c, ArrowJoin c) => ArrowJoin (CokleisliT f c) where
  joinSecond lub f g = lift $ joinSecond lub (f . extract) (unlift g)
  {-# INLINE joinSecond #-}

instance ArrowComplete y c => ArrowComplete y (ConstT r c) where
  f <⊔> g = lift $ \r -> unlift f r <⊔> unlift g r
  {-# INLINE (<⊔>) #-}

instance ArrowJoin c => ArrowJoin (ConstT r c) where
  joinSecond lub f g = lift $ \r -> joinSecond lub f (unlift g r)
  {-# INLINE joinSecond #-}

instance ArrowLowerBounded y c => ArrowLowerBounded y (ConstT r c) where
  bottom = lift $ \_ -> bottom
  {-# INLINE bottom #-}

instance (ArrowApply c, ArrowComplete r c) => ArrowComplete y (ContT r c) where
  (<⊔>) f g = lift $ \k -> unlift f k <⊔> unlift g k

-- instance (ArrowApply c, ArrowJoin c, ArrowComplete r c) => ArrowJoin (ContT r c) where
--   joinSecond _ _ x = lift $ f <⊔> arr g

deriving instance Complete a => ArrowComplete a IO

instance Complete y => ArrowComplete y (ST s) where
  f <⊔> g = lift $ \(# s, x #) -> case unlift f (# s, x #) of
    (# s', y #) -> case unlift g (# s', x #) of
      (# s'' , y' #) -> (# s'', y ⊔ y' #)
  {-# INLINE (<⊔>) #-}

instance (ArrowMonad f c, ArrowComplete (f y) c) => ArrowComplete y (KleisliT f c) where
  f <⊔> g = lift $ unlift f <⊔> unlift g
  {-# INLINE (<⊔>) #-}

instance (ArrowMonad f c, ArrowLowerBounded y c) => ArrowLowerBounded y (KleisliT f c) where
  bottom = lift' bottom
  {-# INLINE bottom #-}

instance ArrowComplete y c => ArrowComplete y (ReaderT r c) where
  f <⊔> g = lift $ unlift f <⊔> unlift g
  {-# INLINE (<⊔>) #-}

instance ArrowJoin c => ArrowJoin (ReaderT r c) where
  joinSecond lub f g = lift $ joinSecond lub (f . snd) (unlift g)
  {-# INLINE joinSecond #-}

instance ArrowLowerBounded y c => ArrowLowerBounded y (ReaderT r c) where
  bottom = ReaderT bottom
  {-# INLINE bottom #-}

instance (ArrowComplete (s,y) c) => ArrowComplete y (StateT s c) where
  f <⊔> g = lift $ unlift f <⊔> unlift g
  {-# INLINE (<⊔>) #-}

instance (ArrowJoin c, Complete s) => ArrowJoin (StateT s c) where
  joinSecond lub f g = lift $ joinSecond (\(s1,y1) (s2,y2) -> (s1 ⊔ s2, lub y1 y2)) (second f) (unlift g)
  {-# INLINE joinSecond #-}

instance (ArrowLowerBounded y c) => ArrowLowerBounded y (StateT s c) where
  bottom = lift' bottom
  {-# INLINE bottom #-}

instance (Applicative f, ArrowComplete y c) => ArrowComplete y (StaticT f c) where
  StaticT f <⊔> StaticT g = StaticT $ (<⊔>) <$> f <*> g 
  {-# INLINE (<⊔>) #-}
  {-# SPECIALIZE instance ArrowComplete y c => ArrowComplete y (StaticT ((->) r) c) #-}

instance (Applicative f, ArrowJoin c) => ArrowJoin (StaticT f c) where
  joinSecond lub f (StaticT g) = StaticT $ joinSecond lub f <$> g
  {-# INLINE joinSecond #-}
  {-# SPECIALIZE instance ArrowJoin c => ArrowJoin (StaticT ((->) r) c) #-}

instance (Applicative f, ArrowLowerBounded y c) => ArrowLowerBounded y (StaticT f c) where
  bottom = StaticT (pure bottom)
  {-# INLINE bottom #-}
  {-# SPECIALIZE instance ArrowLowerBounded y c => ArrowLowerBounded y (StaticT ((->) r) c) #-}

instance (Monoid w, ArrowComplete (w,y) c) => ArrowComplete y (WriterT w c) where
  f <⊔> g = lift $ unlift f <⊔> unlift g
  {-# INLINE (<⊔>) #-}

instance (Monoid w, Complete w, ArrowJoin c) => ArrowJoin (WriterT w c) where
  joinSecond lub f g = lift $ joinSecond (\(w1,y1) (w2,y2) -> (w1 ⊔ w2, lub y1 y2) ) (\x -> (mempty,f x)) (unlift g)
  {-# INLINE joinSecond #-}

instance (Monoid w, ArrowLowerBounded y c) => ArrowLowerBounded y (WriterT w c) where
  bottom = lift' bottom
  {-# INLINE bottom #-}
