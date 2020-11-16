{-# LANGUAGE LambdaCase #-}
{- | Lifts functions from `Data.HashTable.ST.Basic` with `ArrowPrimitive.primitive` -}
module Data.HashTable(HashTable,new,newSized,lookup,insert,update,initialize) where

import           Prelude hiding (lookup)

import           Control.Arrow.Primitive

import           Data.Identifiable
import           Data.HashTable.ST.Basic (HashTable)
import qualified Data.HashTable.ST.Basic as Map

new :: ArrowPrimitive c => c () (HashTable (PrimState c) k v)
new = liftST (const Map.new)
{-# INLINE new #-}

newSized :: ArrowPrimitive c => c Int (HashTable (PrimState c) k v)
newSized = liftST Map.newSized
{-# INLINE newSized #-}

lookup :: (Identifiable k, ArrowPrimitive c) => c (k,HashTable (PrimState c) k v) (Maybe v)
lookup = liftST (\(key,table) -> Map.lookup table key)
{-# INLINE lookup #-}

insert :: (Identifiable k, ArrowPrimitive c) => c (k,v,HashTable (PrimState c) k v) ()
insert = liftST (\(key,val,table) -> Map.insert table key val)
{-# INLINE insert #-}

update :: (Identifiable k, ArrowPrimitive c) => (k -> x -> Maybe v -> (Maybe v,y)) -> c (k,x,HashTable (PrimState c) k v) y
update f = liftST $ \(key,x,table) -> Map.mutate table key $ \m -> f key x m
{-# INLINE update #-}

initialize :: (Identifiable k, ArrowPrimitive c) => c (k,v,HashTable (PrimState c) k v) v
initialize = update $ \_ _new -> \case
  Just old -> (Just old,old)
  Nothing -> (Just _new,_new)
{-# INLINE initialize #-}
