{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Stack where

import Syntax

type CallSignature = (Strategy,[Strat],[TermVar])

class HasStack t c | c -> t where
  stackPush :: StratVar -> CallSignature -> c t t -> c t t

