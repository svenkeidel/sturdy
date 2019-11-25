{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.Closure(Closure,closure,apply,widening) where

import           Control.DeepSeq
import           Control.Arrow
import qualified Control.Arrow.Order as O
import           Control.Arrow.Closure(IsClosure(..))

import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Hashable(Hashable)
import           Data.Identifiable
import           Data.Order
import           Data.Coerce
import           Data.Abstract.Widening
import           Data.Foldable
import           Data.Traversable

import           GHC.Exts
import           Text.Printf

newtype Closure expr env = Closure (HashMap expr env)
  deriving (Eq,Hashable,NFData,Functor)

instance (Identifiable expr, PreOrd env) => PreOrd (Closure expr env) where
  (⊑) = withCls $ \m1 m2 -> M.keysSet m1 ⊑ M.keysSet m2
                         && and (M.intersectionWith (⊑) m1 m2)
  {-# INLINE (⊑) #-}

instance (Identifiable expr, Complete env) => Complete (Closure expr env) where
  (⊔) = withCls $ M.unionWith (⊔)
  {-# INLINE (⊔) #-}

instance IsClosure (Closure expr env) env where
  mapEnvironment f (Closure m) = Closure (M.map f m)
  traverseEnvironment f (Closure m) = Closure <$> traverse f m
  {-# INLINE mapEnvironment #-}
  {-# INLINE traverseEnvironment #-}

instance Foldable (Closure expr) where
  foldMap = foldMapDefault
  {-# INLINE foldMap #-}

instance Traversable (Closure expr) where
  traverse f (Closure m) = Closure <$> traverse f m
  {-# INLINE traverse #-}

instance (Show a,Show b) => Show (Closure a b) where
  show (Closure h)
    | M.null h = "{}"
    | otherwise = "{" ++ init (unwords [ printf "%s -> %s," (show k) (show v) | (k,v) <- M.toList h]) ++ "}"

instance (Identifiable expr, Complete env) => IsList (Closure expr env) where
  type Item (Closure expr env) = (expr,env)
  fromList = Closure . M.fromListWith (⊔)
  toList (Closure cl) = M.toList cl

closure :: Identifiable expr => expr -> env -> Closure expr env
closure expr env = Closure $ M.singleton expr env
{-# INLINE closure #-}

apply :: (O.ArrowComplete y c, ArrowChoice c)
      => c ((expr,env),x) y -> c (Closure expr env,x) y
apply f = proc (Closure m,x) ->
  (| O.joinList (error "encountered empty closure" -< ())
                (\(expr,env) -> f -< ((expr,env),x)) |)
     (M.toList m)
{-# INLINE apply #-}

widening :: Identifiable expr => Widening env -> Widening (Closure expr env)
widening w = withCls $ \m1 m2 ->
  (fold $ M.intersectionWith (\x y -> fst (w x y)) m1 m2,
   M.unionWith (\x y -> snd (w x y)) m1 m2)
{-# INLINE widening #-}

withCls :: Coercible x x' => (HashMap expr env -> x') -> (Closure expr env -> x)
withCls = coerce
{-# INLINE withCls #-}

