{-# LANGUAGE DeriveFunctor #-}
module Data.Concrete.Closure where

import           Control.Monad

import           Data.TreeGrammar.Terminal
import qualified Data.HashSet as H
import qualified Data.Hashable as Hash
import           Data.Identifiable
import           Data.Traversable

data Closure expr env = Closure expr env
  deriving (Show,Eq,Functor)

instance Foldable (Closure expr) where
  foldMap = foldMapDefault

instance Traversable (Closure expr) where
  traverse f (Closure expr env) = Closure expr <$> f env

instance Semigroup (Closure expr env) where
  Closure expr env <> _ = Closure expr env

instance Identifiable expr => Terminal (Closure expr) where
  nonTerminals (Closure _ env) = H.singleton env
  productive _ (Closure _ _) = True
  determinize f (Closure expr env) = Closure expr <$> f (H.singleton env)
  subsetOf f (Closure expr1 env1) (Closure expr2 env2) = do
    guard (expr1 == expr2)
    f (H.singleton [env1],H.singleton [env2])
  map f (Closure expr env) = Closure expr (f env)
  traverse f (Closure expr env) = Closure expr <$> f env
  filter _ cls = cls
  intersection f (Closure expr env1) (Closure _ env2) = Closure expr <$> f (env1,env2)
  hashWithSalt f s (Closure expr env) = f (s `Hash.hashWithSalt` expr) env
