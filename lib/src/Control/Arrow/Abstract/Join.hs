{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Abstract.Join where

import Prelude hiding ((.))

import Control.Arrow
import Data.Order(Complete(..))
import Data.Profunctor

class (Arrow c, Profunctor c) => ArrowJoin c where
  -- | Join two arrow computation with the provided upper bound operator.
  --
  -- Laws:
  -- @
  --   joinWith (⊔) f g = joined f g
  -- @
  joinWith :: (y -> y -> y) -> c x y -> c x y -> c x y

class (Arrow c, Profunctor c) => ArrowTop x c where
  topA :: c () x

joinWith' :: (ArrowJoin c) => (y -> y -> y) -> c x y -> c x' y -> c (x,x') y
joinWith' lub f g = joinWith lub (lmap fst f) (lmap snd g)

(<⊔>) :: (ArrowJoin c, Complete y) => c x y -> c x y -> c x y
(<⊔>) = joinWith (⊔)

-- | Joins a list of arguments. Use it with idiom brackets:
-- @
--   let a = ...; b = ...; xs = ...
--   (| joinList (returnA -< a) (\x -> f -< x+b) |) xs
-- @
joinList :: (ArrowChoice c, ArrowJoin c, Complete y) => c (e,s) y -> c (e,(x,s)) y -> c (e,([x],s)) y
joinList empty f = proc (e,(l,s)) -> case l of
  []     -> empty -< (e,s)
  [x]    -> f -< (e,(x,s))
  (x:xs) -> (f -< (e,(x,s))) <⊔> (joinList empty f -< (e,(xs,s)))

joinList1 :: (ArrowChoice c, ArrowJoin c, Complete y) => c (e,(x,s)) y -> c (e,([x],s)) y
joinList1 f = proc (e,(l,s)) -> case l of
  []     -> returnA -< error "empty list"
  [x]    -> f -< (e,(x,s))
  (x:xs) -> (f -< (e,(x,s))) <⊔> (joinList1 f -< (e,(xs,s)))

instance ArrowJoin (->) where
  joinWith lub f g = \x -> lub (f x) (g x)
