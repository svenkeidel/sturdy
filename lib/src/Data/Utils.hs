module Data.Utils where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

lookupM :: (Ord k, Monoid v) => k -> Map k v -> v
lookupM x m = fromMaybe mempty $ Map.lookup x m

strength1 :: Monad f => (f a,b) -> f (a,b)
strength1 (f,b) = fmap (\a -> (a,b)) f

strength2 :: Monad f => (a,f b) -> f (a,b)
strength2 (a,f) = fmap (\b -> (a,b)) f

costrength1 :: Monad f => Either (f a) b -> f (Either a b)
costrength1 (Left f) = fmap Left f
costrength1 (Right b) = return (Right b)

costrength2 :: Monad f => Either a (f b) -> f (Either a b)
costrength2 (Left a) = return (Left a)
costrength2 (Right f) = fmap Right f

