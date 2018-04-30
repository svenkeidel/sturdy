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
import Data.Fixed (mod')
import Data.List (isPrefixOf)

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
-- number operators
evalOp s ONumPlus [(VNumber a), (VNumber b)] =
    VNumber (a + b)
evalOp s OMul [(VNumber a), (VNumber b)] =
    VNumber (a * b)
evalOp s ODiv [(VNumber a), (VNumber b)] =
    VNumber (a / b)
evalOp s OMod [(VNumber a), (VNumber b)] =
    VNumber (mod' a b)
evalOp s OSub [(VNumber a), (VNumber b)] =
    VNumber (a - b)
evalOp s OLt [(VNumber a), (VNumber b)] =
    VBool (a < b)

-- string operators
evalOp s OStrPlus [(VString a), (VString b)] = 
    VString (a ++ b)
evalOp s OStrLt [(VString a), (VString b)] =
    VBool (a < b)
evalOp s OStrLen [(VString a)] =
    VNumber $ fromIntegral $ length a
evalOp s OStrStartsWith [(VString a), (VString b)] =
    VBool $ isPrefixOf b a

-- boolean operators
evalOp s OBAnd [(VBool a), (VBool b)] = 
    VBool (a && b)
evalOp s OBOr [(VBool a), (VBool b)] = 
    VBool (a || b)
evalOp s OBXOr [(VBool a), (VBool b)] = 
    VBool (a /= b)
evalOp s OBNot [(VBool a)] = 
    VBool (not a)

-- isPrimitive operator
evalOp s OIsPrim [(VNumber _)]  = VBool True
evalOp s OIsPrim [(VString _)]  = VBool True
evalOp s OIsPrim [(VBool _)]    = VBool True
evalOp s OIsPrim [(VNull)]      = VBool True
evalOp s OIsPrim [(VUndefined)] = VBool True
evalOp s OIsPrim [(VObject _)]  = VBool False

-- primitive to number operator
evalOp s OPrimToNum [(VNumber a)] = VNumber a
evalOp s OPrimToNum [(VString st)] = VNumber (read st :: Double)
evalOp s OPrimToNum [(VBool b)] = if b then VNumber 1.0 else VNumber 0.0
evalOp s OPrimToNum [(VNull)] = VNumber 0
evalOp s OPrimToNum [(VUndefined)] = VNumber (0/0)
-- #todo object conversions -> valueOf call

-- primitive to string operator
evalOp s OPrimToStr [(VNumber a)]  = VString $ show a
evalOp s OPrimToStr [(VString st)] = VString st
evalOp s OPrimToStr [(VBool b)]    = VString $ show b
evalOp s OPrimToStr [(VNull)]      = VString "null"
evalOp s OPrimToStr [(VUndefined)] = VString "undefined"
evalOp s OPrimToStr [(VObject _)]  = VString "object"

-- primitive to bool operator
evalOp s OPrimToBool [(VNumber a)]  = VBool $ (a /= 0.0) && (not (isNaN a))
evalOp s OPrimToBool [(VString st)] = VBool $ not $ st == ""
evalOp s OPrimToBool [(VBool b)]    = VBool b
evalOp s OPrimToBool [(VNull)]      = VBool False
evalOp s OPrimToBool [(VUndefined)] = VBool False
evalOp s OPrimToBool [(VObject _)]  = VBool True

-- typeOf operator
evalOp s OTypeof [(VNumber _)]   = VString "number"
evalOp s OTypeof [(VString _)]   = VString "string"
evalOp s OTypeof [(VBool _)]     = VString "boolean"
evalOp s OTypeof [(VUndefined)]  = VString "undefined"
evalOp s OTypeof [(VNull)]       = VString "object"
evalOp s OTypeof [(VLambda _ _)] = VString "function"
evalOp s OTypeof [(VObject _)]   = VString "object"


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

