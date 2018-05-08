{-# LANGUAGE Arrows #-}
module ConcreteArrow where

import Syntax

import Control.Category
import Control.Arrow

import Data.Fixed (mod')
import Data.List (isPrefixOf, find)
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

evalOp :: ConcreteArr (Op, [Value]) Value
evalOp = proc (op, vals) -> case (op, vals) of
    -- number operators
    (ONumPlus, [(VNumber a), (VNumber b)]) -> returnA -< VNumber (a + b)
    (OMul, [(VNumber a), (VNumber b)]) -> returnA -< VNumber (a * b)
    (ODiv, [(VNumber a), (VNumber b)]) -> returnA -< VNumber (a / b)
    (OMod, [(VNumber a), (VNumber b)]) -> returnA -< VNumber (mod' a b)
    (OSub, [(VNumber a), (VNumber b)]) -> returnA -< VNumber (a - b)
    (OLt, [(VNumber a), (VNumber b)]) -> returnA -< VBool (a < b)
    (OToInteger, [(VNumber a)]) -> returnA -< VNumber $ fromInteger (truncate a)
    (OToInt32, [(VNumber a)]) -> 
        returnA -< (let n = mod (truncate a) (2^32 :: Integer) in
            if n > (2^31) then VNumber $ fromInteger $ n - (2^32)
            else VNumber $ fromInteger $ n)
    (OToUInt32, [(VNumber a)]) -> returnA -< VNumber $ fromInteger $ mod (abs $ truncate a) (2^32)
    -- shift operators
    (OLShift, [(VNumber a), (VNumber b)]) ->
        returnA -< VNumber $ fromInteger $ shift (truncate a) (truncate b)
    (OSpRShift, [(VNumber a), (VNumber b)]) -> 
        returnA -< VNumber $ fromInteger $ shift (truncate a) (- (truncate b))
    (OZfRShift, [(VNumber a), (VNumber b)]) -> 
        returnA -< VNumber $ fromInteger $ shift (fromIntegral $ (truncate a :: Word32)) (- (truncate b))
    -- string operators
    (OStrPlus, [(VString a), (VString b)]) -> returnA -< VString (a ++ b)
    (OStrLt, [(VString a), (VString b)]) -> returnA -< VBool (a < b)
    (OStrLen, [(VString a)]) -> returnA -< VNumber $ fromIntegral $ length a
    (OStrStartsWith, [(VString a), (VString b)]) -> returnA -< VBool $ isPrefixOf b a
    -- boolean operators
    (OBAnd, [(VBool a), (VBool b)]) -> returnA -< VBool (a && b)
    (OBOr, [(VBool a), (VBool b)]) -> returnA -< VBool (a || b)
    (OBXOr, [(VBool a), (VBool b)]) -> returnA -< VBool (a /= b)
    (OBNot, [(VBool a)]) -> returnA -< VBool (not a)
    -- isPrimitive operator
    (OIsPrim, [a]) -> returnA -< (case a of
        (VNumber _) -> VBool True
        (VString _) -> VBool True
        (VBool _) -> VBool True
        (VNull) -> VBool True
        (VUndefined) -> VBool True
        (VObject _) -> VBool False)
    -- primToNum operator
    -- #todo object conversions -> valueOf call
    (OPrimToNum, [a]) -> returnA -< (case a of
        (VNumber a) -> VNumber a
        (VString s) -> VNumber $ (read s :: Double)
        (VBool b) -> if b then VNumber 1.0 else VNumber 0.0
        (VNull) -> VNumber 0
        (VUndefined) -> VNumber (0/0))
    -- primToStr operator
    (OPrimToStr, [a]) -> returnA -< (case a of
        (VNumber a) -> VString $ show a
        (VString s) -> VString s
        (VBool b) -> VString $ show b
        (VNull) -> VString "null"
        (VUndefined) -> VString "undefined"
        (VObject _) -> VString "object")
    -- primToBool operator
    (OPrimToBool, [a]) -> returnA -< (case a of 
        (VNumber a) -> VBool $ (a /= 0.0) && (not (isNaN a))
        (VString s) -> VBool $ not $ s == ""
        (VBool b) -> VBool b
        (VNull) -> VBool False
        (VUndefined) -> VBool False
        (VObject _) -> VBool True)
    -- typeOf operator
    (OTypeof, [a]) -> returnA -< (case a of
        (VNumber _) -> VString "number"
        (VString _) -> VString "string"
        (VBool _) -> VString "boolean"
        (VUndefined) -> VString "undefined"
        (VNull) -> VString "object"
        (VLambda _ _) -> VString "function"
        (VObject _) -> VString "object")
    -- equality operators
    (OStrictEq, [a, b]) -> returnA -< VBool $ a == b
    (OAbstractEq, [(VNumber a), (VString b)]) -> do
        res <- evalOp -< (OPrimToNum, [VString b])
        returnA -< VBool $ (VNumber a) == res
    (OAbstractEq, [(VString a), (VNumber b)]) -> do
        res <- evalOp -< (OPrimToNum, [VString a])
        returnA -< VBool $ (VNumber b) == res
    (OAbstractEq, [(VBool a), (VNumber b)]) -> do
        res <- evalOp -< (OPrimToNum, [VBool a])
        returnA -< VBool $ (VNumber b) == res
    (OAbstractEq, [(VNumber a), (VBool b)]) -> do
        res <- evalOp -< (OPrimToNum, [VBool b])
        returnA -< VBool $ (VNumber a) == res
    (OAbstractEq, a) -> returnA -< (case a of
        [(VNumber a), (VNumber b)] -> VBool $ a == b
        [(VNull), (VUndefined)] -> VBool True
        [(VUndefined), (VNull)] -> VBool True)
    -- math operators
    (OMathExp, [(VNumber a)]) -> returnA -< VNumber $ exp a
    (OMathLog, [(VNumber a)]) -> returnA -< VNumber $ log a
    (OMathCos, [(VNumber a)]) -> returnA -< VNumber $ cos a
    (OMathSin, [(VNumber a)]) -> returnA -< VNumber $ sin a
    (OMathAbs, [(VNumber a)]) -> returnA -< VNumber $ abs a
    (OMathPow, [(VNumber a), (VNumber b)]) -> returnA -< VNumber $ a ** b
    -- object operators
    (OHasOwnProp, [(VObject fields), (VString field)]) -> 
        returnA -< VBool $ any (\(name, value) -> (name == field)) fields

getField :: ConcreteArr (Value, Value) Value
getField = proc (VObject fields, VString fieldName) -> 
    let fieldV = find (\(fn, fv) -> fn == fieldName) fields in
        case fieldV of
            -- E-GetField
            Just (n, v) -> returnA -< v
            Nothing -> 
                let protoFieldV = find (\(fn, fv) -> fn == "__proto__") fields in
                    case protoFieldV of
                        -- E-GetField-Proto-Null
                        Just (pn, VUndefined) -> returnA -< VUndefined
                        -- E-GetField-Proto
                        Just (pn, pv) -> do
                            res <- getField -< (pv, VString fieldName)
                            returnA -< res
                        -- E-GetField-NotFound
                        Nothing -> returnA -< VUndefined

deleteField :: ConcreteArr (Value, Value) Value
deleteField = proc (VObject fields, VString name) -> do
    filtered <- arr (\(n, fs) -> filter (\(fn, _) -> fn /= n) fs) -< (name, fields) 
    returnA -< VObject filtered

updateField :: ConcreteArr (Value, Value, Value) Value
updateField = proc (VObject fields, VString name, value) -> do
    -- remove field from obj
    filtered <- deleteField -< (VObject fields, VString name)
    case filtered of
        VObject obj -> do
            -- add field with new value to obj
            newFields <- arr (\(fs, n, v) -> (n, v) : fs) -< (obj, name, value) 
            returnA -< VObject newFields
        _ -> throw -< "Error: deleteField returned non-object value"

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
        res <- evalOp -< (op, vals)
        returnA -< res
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
    EIf cond thenBranch elseBranch -> do
        testRes <- eval -< cond
        case testRes of
            VBool True -> do
                res <- eval -< thenBranch
                returnA -< res
            VBool False -> do
                res <- eval -< elseBranch
                returnA -< res
    EGetField objE fieldE -> do
        fieldV <- eval -< fieldE
        objV <- eval -< objE
        res <- getField -< (objV, fieldV)
        returnA -< res
    EUpdateField objE nameE fieldE -> do
        nameV <- eval -< nameE
        fieldV <- eval -< fieldE
        objV <- eval -< objE
        res <- updateField -< (objV, nameV, fieldV)
        returnA -< res
    EDeleteField objE nameE -> do
        objV <- eval -< objE
        nameV <- eval -< nameE
        res <- deleteField -< (objV, nameV)
        returnA -< res
    ESeq f s -> do
        res <- (first eval) Control.Category.. (second eval) -< (f, s)
        returnA -< snd res
    EWhile t b -> do
        tres <- eval -< t
        case tres of
            VBool True -> do
                res <- eval -< (EWhile t b) 
                returnA -< res
            VBool False ->
                returnA -< VUndefined
            _ -> throw -< "Error: Non bool value in test of while loop"
    -- ESetRef r v -> do
    EEval -> throw -< "Eval expression encountered, aborting"