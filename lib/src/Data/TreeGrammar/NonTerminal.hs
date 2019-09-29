{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Data.TreeGrammar.NonTerminal(NonTerminal(..),fresh',Named,getName) where

import Control.Monad.State
import Data.Text(Text,pack,unpack)
import Data.Hashable
import Text.Printf

class Show n => NonTerminal n where
  type Gen n :: *
  fresh :: Maybe String -> State (Gen n) n
  generate :: forall (g :: * -> (* -> *) -> *) t.
              State (Gen n) (g n t) -> g n t

instance NonTerminal Int where
  type Gen Int = Int
  fresh _ = do
    x <- get
    put (x+1)
    return x
  generate f = evalState f 0

data Named = Named (Maybe Text) Int
getName :: Named -> Text
getName (Named (Just n) _) = n
getName (Named _ n) = pack (show n)

instance Eq Named where
  Named _ n == Named _ m = n == m
instance Hashable Named where
  hashWithSalt s (Named _ n) = hashWithSalt s n
instance Show Named where
  show (Named (Just n) _) = printf "%s" (unpack n)
  show (Named Nothing m) = show m

instance NonTerminal Named where
  type Gen Named = Int
  fresh m = do
    n <- fresh m
    return (Named (pack <$> m) n)
  generate f = evalState f 0

fresh' :: NonTerminal n => Maybe String -> State (Gen n,x) n
fresh' s = state (\(i,x) -> let (f,i') = runState (fresh s) i in (f,(i',x)))
