{-# LANGUAGE Arrows #-}
module Control.Arrow.Order where

import           Prelude hiding ((.))

import           Control.Arrow
import           Data.Order(Complete(..))
import           Data.Profunctor

class (Arrow c, Profunctor c) => ArrowLowerBounded c where
  bottom :: c x y

class (Arrow c, Profunctor c) => ArrowComplete c where
  -- | Join two arrow computation with the provided upper bound operator.
  join :: (y -> y -> y) -> c x y -> c x y -> c x y

join' :: (ArrowComplete c) => (y -> y -> y) -> c x y -> c x' y -> c (x,x') y
join' lub f g = join lub (lmap fst f) (lmap snd g)

(<⊔>) :: (ArrowComplete c, Complete y) => c x y -> c x y -> c x y
(<⊔>) = join (⊔)

-- | Joins a list of arguments. Use it with idiom brackets:
-- @
--   let a = ...; b = ...; xs = ...
--   (| joinList (returnA -< a) (\x -> f -< x+b) |) xs
-- @
joinList :: (ArrowChoice c, ArrowComplete c, Complete y) => c (e,s) y -> c (e,(x,s)) y -> c (e,([x],s)) y
joinList empty f = proc (e,(l,s)) -> case l of
  []     -> empty -< (e,s)
  [x]    -> f -< (e,(x,s))
  (x:xs) -> (f -< (e,(x,s))) <⊔> (joinList empty f -< (e,(xs,s)))

joinList1 :: (ArrowChoice c, ArrowLowerBounded c, ArrowComplete c, Complete y) => c (e,(x,s)) y -> c (e,([x],s)) y
joinList1 f = proc (e,(l,s)) -> case l of
  []     -> bottom -< ()
  [x]    -> f -< (e,(x,s))
  (x:xs) -> (f -< (e,(x,s))) <⊔> (joinList1 f -< (e,(xs,s)))

instance ArrowComplete (->) where
  join lub f g = \x -> lub (f x) (g x)
