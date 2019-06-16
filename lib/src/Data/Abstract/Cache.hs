{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Abstract.Cache where

import           Prelude hiding (lookup)
   
import           Data.Identifiable
import           Data.Order
import           Data.Abstract.Widening
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M

data RecomputeOrCached a b = Recompute a | Cached b
type CachingStrategy cache a b = a -> cache a b -> (RecomputeOrCached a b,cache a b)

class (Identifiable a, LowerBounded b) => IsCache cache a b where 
  empty :: cache a b
  lookup :: a -> cache a b -> Maybe (Stable,b)
  lookupInit :: a -> cache a b -> (b,cache a b)
  update :: Widening b -> a -> b -> cache a b -> ((Stable,b),cache a b)

newtype Cache a b = Cache (HashMap a (Stable,b))
instance (Identifiable a, LowerBounded b) => IsCache Cache a b where
  empty = Cache M.empty
  lookup a (Cache m) = M.lookup a m
  lookupInit a (Cache m) = let m' = M.insertWith (\_ old -> old) a (Instable,bottom) m in (snd (m' M.! a), Cache m')
  update widen x y (Cache cache) = case M.lookup x cache of
    Just (_,yOld) -> let yNew = widen yOld y in (yNew,Cache (M.insert x yNew cache))
    Nothing -> ((Instable,y),Cache (M.insert x (Instable,y) cache))

stabilized :: IsCache cache a b => CachingStrategy cache a b
stabilized a cache = case lookup a cache of
  Just (Stable,b)  -> (Cached b,cache)
  _ -> (Recompute a,cache)

recompute :: IsCache cache a b => CachingStrategy cache a b
recompute a cache = (Recompute a,cache)
