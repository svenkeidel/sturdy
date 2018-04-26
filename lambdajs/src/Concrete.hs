module Concrete 
    ( eval
    , Scope
    , emptyScope
    ) where

import Syntax

import Data.Map (Map, empty)

type Scope = [Map String Value]
emptyScope :: Scope
emptyScope = []

eval :: Scope -> Expr -> Value
eval _ (ENumber d) = VNumber d
eval _ (EString s) = VString s
eval _ (EBool b) = VBool b
eval _ EUndefined = VUndefined
eval _ ENull = VNull
eval _ (ELambda ids exp) = VLambda ids exp
eval s (EObject fields) = VObject (map (\(name, exp) -> (name, eval s exp)) fields)