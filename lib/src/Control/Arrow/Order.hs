{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Order where

import           Prelude hiding ((.))

import           Control.Arrow
import           Data.Order(Complete(..))
import           Data.Profunctor

class (Arrow c, Profunctor c) => ArrowLowerBounded c where
  bottom :: c x y

class (Arrow c, Profunctor c) => ArrowComplete y c where
  (<⊔>) :: c x y -> c x y -> c x y

instance Complete y => ArrowComplete y (->) where
  (<⊔>) f g = \x -> f x ⊔ g x

class (Arrow c, Profunctor c) => ArrowJoin c where
  join :: (y -> y -> y) -> c x y -> c x y -> c x y

instance ArrowJoin (->) where
  join lub f g = \x -> f x `lub` g x

join' :: (ArrowJoin c) => (y -> y -> y) -> c x y -> c x' y -> c (x,x') y
join' lub f g = join lub (lmap fst f) (lmap snd g)

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

joinList1 :: (ArrowChoice c, ArrowLowerBounded c, ArrowComplete y c) => c (e,(x,s)) y -> c (e,([x],s)) y
joinList1 f = proc (e,(l,s)) -> case l of
  []     -> bottom -< ()
  [x]    -> f -< (e,(x,s))
  (x:xs) -> (f -< (e,(x,s))) <⊔> (joinList1 f -< (e,(xs,s)))
