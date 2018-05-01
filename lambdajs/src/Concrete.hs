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
import Data.Bits (shift, bit)
import Data.Word (Word32)

data Scope = Scope (Map String Value) (Maybe Scope)
parent :: Scope -> Maybe Scope
parent (Scope _ p) = p

setParent :: Scope -> Scope -> Scope
setParent (Scope vals _) parent = Scope vals (Just parent)

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

evalOp :: Op -> [Value] -> Value
-- number operators
evalOp ONumPlus [(VNumber a), (VNumber b)] =
    VNumber (a + b)
evalOp OMul [(VNumber a), (VNumber b)] =
    VNumber (a * b)
evalOp ODiv [(VNumber a), (VNumber b)] =
    VNumber (a / b)
evalOp OMod [(VNumber a), (VNumber b)] =
    VNumber (mod' a b)
evalOp OSub [(VNumber a), (VNumber b)] =
    VNumber (a - b)
evalOp OLt [(VNumber a), (VNumber b)] =
    VBool (a < b)
evalOp OToInteger [(VNumber a)] = VNumber $ fromInteger (truncate a)
evalOp OToInt32 [(VNumber a)] = 
    let n = mod (truncate a) ((2^32) :: Integer) in
        if n > (2^31) then VNumber $ fromInteger $ n - (2^32)
        else VNumber $ fromInteger $ n
evalOp OToUInt32 [(VNumber a)] = VNumber $ fromInteger $ mod (abs $ truncate a) (2^32)

-- shift operators
evalOp OLShift [(VNumber a), (VNumber b)] = 
    VNumber $ fromInteger $ shift (truncate a) (truncate b)
evalOp OSpRShift [(VNumber a), (VNumber b)] =
    VNumber $ fromInteger $ shift (truncate a) (- (truncate b))
evalOp OZfRShift [(VNumber a), (VNumber b)] =
    VNumber $ fromInteger $ shift (fromIntegral $ (truncate a :: Word32)) (- (truncate b))

-- string operators
evalOp OStrPlus [(VString a), (VString b)] = 
    VString (a ++ b)
evalOp OStrLt [(VString a), (VString b)] =
    VBool (a < b)
evalOp OStrLen [(VString a)] =
    VNumber $ fromIntegral $ length a
evalOp OStrStartsWith [(VString a), (VString b)] =
    VBool $ isPrefixOf b a

-- boolean operators
evalOp OBAnd [(VBool a), (VBool b)] = 
    VBool (a && b)
evalOp OBOr [(VBool a), (VBool b)] = 
    VBool (a || b)
evalOp OBXOr [(VBool a), (VBool b)] = 
    VBool (a /= b)
evalOp OBNot [(VBool a)] = 
    VBool (not a)

-- isPrimitive operator
evalOp OIsPrim [(VNumber _)]  = VBool True
evalOp OIsPrim [(VString _)]  = VBool True
evalOp OIsPrim [(VBool _)]    = VBool True
evalOp OIsPrim [(VNull)]      = VBool True
evalOp OIsPrim [(VUndefined)] = VBool True
evalOp OIsPrim [(VObject _)]  = VBool False

-- primitive to number operator
evalOp OPrimToNum [(VNumber a)] = VNumber a
evalOp OPrimToNum [(VString st)] = VNumber (read st :: Double)
evalOp OPrimToNum [(VBool b)] = if b then VNumber 1.0 else VNumber 0.0
evalOp OPrimToNum [(VNull)] = VNumber 0
evalOp OPrimToNum [(VUndefined)] = VNumber (0/0)
-- #todo object conversions -> valueOf call

-- primitive to string operator
evalOp OPrimToStr [(VNumber a)]  = VString $ show a
evalOp OPrimToStr [(VString st)] = VString st
evalOp OPrimToStr [(VBool b)]    = VString $ show b
evalOp OPrimToStr [(VNull)]      = VString "null"
evalOp OPrimToStr [(VUndefined)] = VString "undefined"
evalOp OPrimToStr [(VObject _)]  = VString "object"

-- primitive to bool operator
evalOp OPrimToBool [(VNumber a)]  = VBool $ (a /= 0.0) && (not (isNaN a))
evalOp OPrimToBool [(VString st)] = VBool $ not $ st == ""
evalOp OPrimToBool [(VBool b)]    = VBool b
evalOp OPrimToBool [(VNull)]      = VBool False
evalOp OPrimToBool [(VUndefined)] = VBool False
evalOp OPrimToBool [(VObject _)]  = VBool True

-- typeOf operator
evalOp OTypeof [(VNumber _)]   = VString "number"
evalOp OTypeof [(VString _)]   = VString "string"
evalOp OTypeof [(VBool _)]     = VString "boolean"
evalOp OTypeof [(VUndefined)]  = VString "undefined"
evalOp OTypeof [(VNull)]       = VString "object"
evalOp OTypeof [(VLambda _ _)] = VString "function"
evalOp OTypeof [(VObject _)]   = VString "object"

-- equality operators
evalOp OStrictEq [a, b] = VBool $ a == b
evalOp OAbstractEq [(VNumber a), (VNumber b)] = VBool $ a == b
evalOp OAbstractEq [(VNull), (VUndefined)] = VBool True
evalOp OAbstractEq [(VUndefined), (VNull)] = VBool True
evalOp OAbstractEq [(VNumber a), (VString b)] = 
    VBool $ (VNumber a) == (evalOp OPrimToNum [(VString b)])
evalOp OAbstractEq [(VString a), (VNumber b)] =
    VBool $ (evalOp OPrimToNum [(VString a)]) == (VNumber b)
evalOp OAbstractEq [(VBool a), (VNumber b)] =
    VBool $ (evalOp OPrimToNum [(VBool a)]) == (VNumber b)
evalOp OAbstractEq [(VNumber a), (VBool b)] =
    VBool $ (VNumber a) == (evalOp OPrimToNum [(VBool b)])

-- math operators
evalOp OMathExp [(VNumber a)] = VNumber $ exp a
evalOp OMathLog [(VNumber a)] = VNumber $ log a
evalOp OMathCos [(VNumber a)] = VNumber $ cos a
evalOp OMathSin [(VNumber a)] = VNumber $ sin a
evalOp OMathAbs [(VNumber a)] = VNumber $ abs a
evalOp OMathPow [(VNumber a), (VNumber b)] = VNumber $ a ** b

-- object operators
evalOp OHasOwnProp [(VObject fields), (VString field)] =
    VBool $ any (\(name, value) -> (name == field)) fields

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
eval s (EOp op exps) = evalOp op (map (eval s) exps)
-- #todo fix order of argument evaluation
eval s (EApp (ELambda params body) args) = eval scope body
    where scope = Scope (fromList (zip params (map (eval s) args))) (Just s)
        
    