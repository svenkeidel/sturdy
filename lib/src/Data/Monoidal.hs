{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Monoidal where

import Prelude hiding (id,(.))
import Control.Category
import Control.Comonad
import Control.Arrow

class Monoidal m c where
  (⊗) :: (c x x') -> (c y y') -> c (x `m` y) (x' `m` y')
  assoc1 :: c (x `m` (y `m` z)) ((x `m` y) `m` z)
  assoc2 :: c ((x `m` y) `m` z) (x `m` (y `m` z))

instance Monoidal (,) (->) where
  (⊗) f g (x,y) = (f x,g y)
  assoc1 = \(a,(b,c)) -> ((a,b),c)
  assoc2 = \((a,b),c) -> (a,(b,c))
  {-# INLINE (⊗) #-}
  {-# INLINE assoc1 #-}
  {-# INLINE assoc2 #-}

instance Monoidal Either (->) where
  (⊗) f _ (Left x) = Left (f x)
  (⊗) _ g (Right y) = Right (g y)

  assoc1 (Left a) = Left (Left a)
  assoc1 (Right (Left b)) = Left (Right b)
  assoc1 (Right (Right c)) = Right c

  assoc2 (Left (Left a)) = Left a
  assoc2 (Left (Right b)) = Right (Left b)
  assoc2 (Right c) = Right (Right c)
  {-# INLINE (⊗) #-}
  {-# INLINE assoc1 #-}
  {-# INLINE assoc2 #-}

class Strong f m c where
  strength1 :: c (f a `m` b) (f (a `m` b))
  strength2 :: c (a `m` f b) (f (a `m` b))

instance Functor f => Strong f (,) (->) where
  strength1 (f,b) = fmap (\a -> (a,b)) f
  strength2 (a,f) = fmap (\b -> (a,b)) f
  {-# INLINE strength1 #-}
  {-# INLINE strength2 #-}

instance Applicative f => Strong f Either (->) where
  strength1 (Left f) = fmap Left f
  strength1 (Right b) = pure (Right b)
  strength2 (Left a) = pure (Left a)
  strength2 (Right f) = fmap Right f
  {-# INLINE strength1 #-}
  {-# INLINE strength2 #-}

class Costrong f m c where
  costrength1 :: c (f (a `m` b)) (f a `m` b)
  costrength2 :: c (f (a `m` b)) (a `m` f b)
  costrength :: c (f (a `m` b)) (f a `m` f b)

instance Comonad f => Costrong f (,) (->) where
  costrength1 f = (fmap fst f,snd (extract f))
  costrength2 f = (fst (extract f),fmap snd f)
  costrength f = (fmap fst f,fmap snd f)
  {-# INLINE costrength1 #-}
  {-# INLINE costrength2 #-}
  {-# INLINE costrength #-}

instance Comonad f => Costrong f Either (->) where
  costrength1 f = case extract f of
    Left a -> Left (fmap (id ||| const a) f)
    Right b -> Right b
  costrength2 f = case extract f of
    Left a -> Left a
    Right b -> Right (fmap (const b ||| id) f)
  costrength f = case extract f of
    Left a -> Left (fmap (id ||| const a) f)
    Right b -> Right (fmap (const b ||| id) f)

class Monoidal m c => Symmetric m c where
  commute :: c (a `m` b) (b `m` a)

instance Symmetric (,) (->) where
  commute (a,b) = (b,a)
  {-# INLINE commute #-}

instance Symmetric Either (->) where
  commute (Left a) = Right a
  commute (Right a) = Left a
  {-# INLINE commute #-}

class Distributive m n c where
  distribute1 :: c (x `m` (y `n` z)) ((x `m` y) `n` (x `m` z))
  distribute2 :: c ((x `m` y) `n` (x `m` z)) (x `m` (y `n` z))

instance Distributive (,) Either (->) where
  distribute1 (a,x) = case x of
    Left b -> Left (a,b)
    Right c -> Right (a,c)
  {-# INLINE distribute1 #-}

  distribute2 (Left (a,b)) = (a,Left b)
  distribute2 (Right (a,c)) = (a,Right c)
  {-# INLINE distribute2 #-}

shuffle1 :: (a,(b,c)) -> (b,(a,c))
shuffle1 (a,(b,c)) = (b,(a,c))
{-# INLINE shuffle1 #-}

shuffle2 :: (b,(a,c)) -> (a,(b,c))
shuffle2 (b,(a,c)) = (a,(b,c))
{-# INLINE shuffle2 #-}

shuffle3 :: ((a,b),c) -> (b,(a,c))
shuffle3 ((a,b),c) = (b,(a,c))
{-# INLINE shuffle3 #-}

data Iso c a b = Iso
  { from :: c a b
  , to   :: c b a
  }

instance Category c => Category (Iso c) where
  id = Iso { from = id, to = id }
  iso1 . iso2 = Iso { from = from iso1 . from iso2, to = to iso2 . to iso1 }
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance Monoidal m c => Monoidal m (Iso c) where
  iso1 ⊗ iso2 = Iso
    { from = from iso1 ⊗ from iso2
    , to = to iso1 ⊗ to iso2
    }
  assoc1 = Iso { from = assoc1, to = assoc2 }
  assoc2 = Iso { from = assoc2, to = assoc1 }
  {-# INLINE (⊗) #-}
  {-# INLINE assoc1 #-}
  {-# INLINE assoc2 #-}

instance Symmetric m c => Symmetric m (Iso c) where
  commute = Iso { from = commute, to = commute }
  {-# INLINE commute #-}

instance Distributive m n c => Distributive m n (Iso c) where
  distribute1 = Iso { from = distribute1, to = distribute2 }
  distribute2 = Iso { from = distribute2, to = distribute1 }
  {-# INLINE distribute1 #-}
  {-# INLINE distribute2 #-}
