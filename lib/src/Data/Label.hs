{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.Label where

import Data.Hashable
import Data.Order
import Data.Abstract.FreeCompletion
import Control.Monad.State
import Control.DeepSeq
import Text.Printf
import Data.Text.Prettyprint.Doc

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

-- Retrieves label from expression.
class HasLabel x where
  label :: x -> Label

newtype Label = Label { labelVal :: Int }
  deriving (Ord,Eq,Hashable,Num,NFData,Generic,ToJSON,FromJSON)

instance Show Label where
  show (Label l) = printf "#%d" l

instance Pretty Label where
  pretty (Label l) = "#" <> pretty l

instance Enum Label where
  toEnum = Label
  fromEnum = labelVal

instance PreOrd Label where
  (⊑) = (==)

instance Complete (FreeCompletion Label) where
  Lower l1 ⊔ Lower l2 | l1 == l2 = Lower l1
  _ ⊔ _ = Top

instance UpperBounded (FreeCompletion Label) where
  top = Top

fresh :: MonadState Label m => m Label
fresh = state (\l -> (l,l+1))

generate :: State Label x -> x
generate = generateFrom 0

generateState :: State Label x -> (x, Label)
generateState = generateStateFrom 0

generateFrom :: Label -> State Label x -> x
generateFrom i m = evalState m i

generateStateFrom :: Label -> State Label x -> (x, Label)
generateStateFrom i m = runState m i

generate' :: Monad m => StateT Label m x -> m x
generate' = generateFrom' 0

generateState' :: StateT Label m x -> m (x, Label)
generateState' = generateStateFrom' 0

generateFrom' :: Monad m => Label -> StateT Label m x -> m x
generateFrom' i m = evalStateT m i

generateStateFrom' :: Label -> StateT Label m x -> m (x, Label)
generateStateFrom' i m = runStateT m i
