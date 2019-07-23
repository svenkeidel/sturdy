{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.Closure(Closure,closure,apply,widening) where

import           Control.DeepSeq
import           Control.Arrow
import qualified Control.Arrow.Order as O

import           Data.Profunctor
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Hashable(Hashable)
import           Data.Identifiable
import           Data.Order
import           Data.Coerce
import           Data.Abstract.Widening
import           Data.Foldable

import           Text.Printf

newtype Closure expr env = Closure (HashMap expr env)
  deriving (Eq,Hashable,NFData)

instance (Identifiable expr, PreOrd env) => PreOrd (Closure expr env) where
  (⊑) = withCls $ \m1 m2 -> and $ M.intersectionWith (⊑) m1 m2

instance (Identifiable expr, Complete env) => Complete (Closure expr env) where
  (⊔) = withCls $ M.unionWith (⊔)

instance (Show a,Show b) => Show (Closure a b) where
  show (Closure h)
    | M.null h = "{}"
    | otherwise = "{" ++ init (unwords [ printf "%s -> %s," (show k) (show v) | (k,v) <- M.toList h]) ++ "}"

closure :: Identifiable expr => expr -> env -> Closure expr env
closure expr env = Closure $ M.singleton expr env

apply :: (O.ArrowComplete c, O.ArrowLowerBounded c, ArrowChoice c, Profunctor c, Complete y)
      => c (e,((expr,env),s)) y -> c (e,(Closure expr env,s)) y
apply f = lmap (second $ first $ withCls M.toList) (O.joinList1 f)

widening :: Identifiable expr => Widening env -> Widening (Closure expr env)
widening w = withCls $ \m1 m2 ->
  (fold $ M.intersectionWith (\x y -> fst (w x y)) m1 m2,
   M.unionWith (\x y -> snd (w x y)) m1 m2)

withCls :: Coercible x x' => (HashMap expr env -> x') -> (Closure expr env -> x)
withCls = coerce
{-# INLINE withCls #-}

