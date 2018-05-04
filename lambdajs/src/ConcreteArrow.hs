{-# LANGUAGE Arrows #-}
module ConcreteArrow where

import Syntax

import Control.Category
import Control.Arrow

import Data.Fixed (mod')
import Data.List (isPrefixOf)
import Data.Bits (shift, bit)
import Data.Word (Word32)

import Data.Map (Map, findWithDefault)
import qualified Data.Map as Map

type Store = Map String Value
lookup :: String -> Store -> Maybe Value
lookup s st = Map.lookup s st 
empty :: Store
empty = Map.empty
insert :: String -> Value -> Store -> Store
insert s v st = Map.insert s v st


newtype ConcreteArr x y = ConcreteArr {runArr :: x -> Store -> Either String (Store, y) } 

instance Category ConcreteArr where
    id = ConcreteArr (\x st -> Right (st, x))
    ConcreteArr f . ConcreteArr g = ConcreteArr $ \x st -> case g x st of
        Left er -> Left er
        Right (st', y) -> f y st'

instance Arrow ConcreteArr where
    arr f = ConcreteArr (\x st -> Right (st, f x))
    first (ConcreteArr f) = ConcreteArr $ \(x, y) st -> case f x st of
        Left er -> Left er
        Right (st', z) -> Right (st', (z, y))

instance ArrowChoice ConcreteArr where
    left (ConcreteArr f) = ConcreteArr $ \e st -> case e of
        Left x -> case f x st of
            Left err -> Left err
            Right (st', y) -> Right (st', Left y)
        Right z -> Right (st, Right z)

-- Constructs an arrow that operates on a list of inputs producing a list of outputs.
-- Used for evaluating a list of arguments, producing a list of values which are passed to the evalOp function.
mapA :: ArrowChoice arr => arr a b -> arr [a] [b]
mapA f = arr (\list -> case list of 
    [] -> Left ()
    (x : xs) -> Right (x, xs)) >>> (arr (const []) ||| ((f *** mapA f) >>> (arr (uncurry (:)))))

get :: ConcreteArr () Store
get = ConcreteArr (\() st -> Right (st, st))

put :: ConcreteArr Store ()
put = ConcreteArr (\st _ -> Right (st, ()))

throw :: ConcreteArr String a
throw = ConcreteArr (\er _ -> Left er)

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

eval :: ConcreteArr Expr Value
eval = proc e -> case e of
    ENumber d -> returnA -< VNumber d
    EString s -> returnA -< VString s
    EBool b -> returnA -< VBool b
    EUndefined -> returnA -< VUndefined
    ENull -> returnA -< VNull
    ELambda ids exp -> returnA -< VLambda ids exp
    EObject fields -> do
        vals <- (mapA $ second eval) -< fields
        returnA -< VObject vals
    EId id -> do
        st <- get -< ()
        case Map.lookup id st of
            Just v -> returnA -< v
            Nothing -> returnA -< VUndefined
    EOp op exps -> do
        vals <- mapA eval -< exps
        returnA -< evalOp op vals
    EApp (ELambda params body) args -> do
        vals <- mapA eval -< args
        zipped <- arr $ uncurry zip -< (params, vals)

        scope <- get -< ()
        put -< Map.fromList $ (Map.toList scope) ++ (zipped)

        res <- eval -< body 
        returnA -< res
    ELet vars body -> do
        res <- eval -< EApp (ELambda (map fst vars) body) (map snd vars)
        returnA -< res
