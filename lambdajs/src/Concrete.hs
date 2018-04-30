module Concrete 
    ( eval
    , Scope (..)
    , emptyScope
    , fromList
    , insert
    , parent
    , values
    ) where

import Syntax

import Data.Map (Map, empty, findWithDefault, lookup, fromList, insert)
import qualified Data.Map as Map

data Scope = Scope (Map String Value) (Maybe Scope)
parent :: Scope -> Maybe Scope
parent (Scope _ p) = p

values :: Scope -> Map String Value
values (Scope v _) = v

get :: Scope -> String -> Maybe Value
get (Scope values parent) field = 
    case Map.lookup field values of
        Nothing -> case parent of
            Nothing -> Nothing
            Just p -> get p field
        Just val -> Just val

emptyScope :: Scope
emptyScope = Scope empty Nothing

evalOp :: Scope -> Op -> [Value] -> Value
evalOp s ONumPlus [(VNumber a), (VNumber b)] =
    VNumber (a + b)
evalOp s OStrPlus [(VString a), (VString b)] = 
    VString (a ++ b)    

eval :: Scope -> Expr -> Value
eval _ (ENumber d) = VNumber d
eval _ (EString s) = VString s
eval _ (EBool b) = VBool b
eval _ EUndefined = VUndefined
eval _ ENull = VNull
eval _ (ELambda ids exp) = VLambda ids exp
-- #todo #test fix fields referencing each other(?)
eval s (EObject fields) = 
    VObject (map (\(name, exp) -> (name, eval s exp)) fields)
eval s (EId id) = case get s id of 
    Nothing -> VUndefined
    Just val -> val
eval s (EOp op exps) = evalOp s op (map (eval s) exps)

