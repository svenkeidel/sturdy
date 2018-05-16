{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
module ValueInterpreter where
import Prelude hiding(lookup, read)
import qualified Prelude(read)

import Syntax

import Data.Concrete.Error
import Data.Concrete.Store
import Data.Concrete.Environment
import Control.Category
import Control.Arrow
import Control.Arrow.Transformer.Concrete.Except
import Control.Arrow.Transformer.Concrete.Environment
import Control.Arrow.Transformer.Concrete.Store
import Control.Arrow.Fail
import Control.Arrow.Try
import Control.Arrow.Reader
import Control.Arrow.Environment
import Control.Arrow.State
import Control.Arrow.Transformer.State
import Control.Arrow.Store
import Control.Arrow.Utils (foldA)
import Data.Identifiable
import Data.Fixed (mod')
import Data.List (isPrefixOf, find)
import Data.Bits (shift, bit)
import Data.Word (Word32)

import Data.Map (Map, findWithDefault)
import qualified Data.Map as Map

newtype LJSArrow x y = LJSArrow (Environment Ident Location (Except String (StoreArrow Location Value (->))) x y)
deriving instance ArrowChoice LJSArrow
deriving instance Arrow LJSArrow
deriving instance Category LJSArrow
deriving instance ArrowFail String LJSArrow
deriving instance ArrowEnv Ident Location (Env Ident Location) LJSArrow


instance (Show Location, Identifiable Value, ArrowChoice c) => ArrowStore Location Value lab (StoreArrow Location Value c) where
  read =
    StoreArrow $ State $ proc (s,(var,_)) -> case Data.Concrete.Store.lookup var s of
      Success v -> returnA -< (s,v)
      Fail _ -> returnA -< (s, VUndefined)
  write = StoreArrow (State (arr (\(s,(x,v,_)) -> (Data.Concrete.Store.insert x v s,()))))

deriving instance ArrowStore Location Value () LJSArrow

runLJS :: LJSArrow x y -> [(Ident, Location)] -> [(Location, Value)] -> x -> (Store Location Value, Error String y)
runLJS (LJSArrow f) env env2 x = runStore (runExcept (runEnvironment f)) (Data.Concrete.Store.fromList env2, (env, x))

runConcrete :: [(Ident, Location)] -> [(Location, Value)] -> Expr -> (Store Location Value, Error String Value)
runConcrete = runLJS eval

-- Constructs an arrow that operates on a list of inputs producing a list of outputs.
-- Used for evaluating a list of arguments, producing a list of values which are passed to the evalOp function.
mapA :: ArrowChoice arr => arr a b -> arr [a] [b]
mapA f = arr (\list -> case list of 
    [] -> Left ()
    (x : xs) -> Right (x, xs)) >>> (arr (const []) ||| ((f *** mapA f) >>> (arr (uncurry (:)))))

evalOp :: (ArrowFail String c, ArrowChoice c) => c (Op, [Value]) Value
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
        (VString s) -> VNumber $ (Prelude.read s :: Double)
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

getField :: (ArrowFail String c, ArrowChoice c) => c (Value, Value) Value
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

deleteField :: ArrowFail String e => e (Value, Value) Value
deleteField = proc (VObject fields, VString name) -> do
    filtered <- arr (\(n, fs) -> filter (\(fn, _) -> fn /= n) fs) -< (name, fields) 
    returnA -< VObject filtered

updateField :: (ArrowFail String e, ArrowChoice e) => e (Value, Value, Value) Value
updateField = proc (VObject fields, VString name, value) -> do
    -- remove field from obj
    filtered <- deleteField -< (VObject fields, VString name)
    case filtered of
        VObject obj -> do
            -- add field with new value to obj
            newFields <- arr (\(fs, n, v) -> (n, v) : fs) -< (obj, name, value) 
            returnA -< VObject newFields
        _ -> failA -< "Error: deleteField returned non-object value"

eval :: (ArrowFail String c, ArrowEnv Ident Location (Env Ident Location) c, ArrowStore Location Value () c, ArrowChoice c) => c Expr Value
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
        loc <- Control.Arrow.Environment.lookup -< id
        returnA -< VRef loc
    EOp op exps -> do
        vals <- mapA eval -< exps
        res <- evalOp -< (op, vals)
        returnA -< res
    EApp (ELambda params body) args -> do
        -- #todo generate fresh, this now overrides previous assignments
        -- look at the environment and find 'length x' unused numbers starting from 1
        -- this also garbage collects
        range <- arr $ (\x -> [1..(length x)]) -< params
        locations <- arr $ map (\x -> Location x) -< range

        vals <- mapA eval -< args
        forStore <- arr $ uncurry zip -< (locations, vals)
        mapA write -< map (\(a, b) -> (a, b, ())) forStore

        forEnv <- arr $ uncurry zip -< (params, locations)
        scope <- getEnv -< ()
        env' <- bindings -< (forEnv, scope)
        res <- localEnv eval -< (env', body)

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
        res1 <- eval -< f
        case res1 of
            VThrown v -> returnA -< VThrown v
            VBreak l v -> returnA -< VBreak l v
            _ -> do 
                res2 <- eval -< s
                returnA -< res2
    EWhile t b -> do
        tres <- eval -< t
        case tres of
            VBool True -> do
                bres <- eval -< b
                case bres of
                    VBreak l v -> returnA -< VBreak l v 
                    VThrown v -> returnA -< VThrown v 
                    _ -> do
                        res <- eval -< (EWhile t b) 
                        returnA -< res
            VBool False ->
                returnA -< VUndefined
            _ -> failA -< "Error: Non bool value in test of while loop"
    ESetRef locE valE -> do
        loc <- eval -< locE
        val <- eval -< valE
        case loc of
            VRef l -> do
                write -< (l, val, ())
                returnA -< VUndefined
            _ -> failA -< "Error: ESetRef lhs did not evaluate to a location"
        
    EDeref locE -> do
        loc <- eval -< locE
        case loc of 
            VRef l -> do
                val <- read -< (l, ())
                returnA -< val
            _ -> failA -< "Error: EDeref lhs did not evaluate to a location"
    --ERef val -> do
    EEval -> failA -< "Eval expression encountered, aborting"
    ELabel l e -> do
        res <- eval -< e
        case res of
            VBreak l1 v -> case l1 == l of
                True -> returnA -< v
                False -> returnA -< VBreak l1 v
            v -> returnA -< v
    EBreak l e -> do
        res <- eval -< e
        returnA -< VBreak l res
    EThrow e -> do
        res <- eval -< e
        returnA -< VThrown res
    ECatch try catch -> do
        res1 <- eval -< try
        case res1 of
            VThrown v -> do
                case catch of
                    ELambda [x] body -> do
                        scope <- getEnv -< ()
                        env' <- extendEnv -< (x, Location 0, scope)
                        write -< (Location 0, v, ())
                        res <- localEnv eval -< (env', body)
                        returnA -< res
                    _ -> failA -< "Error: Catch block must be of type ELambda"
            v-> returnA -< v
    EFinally b1 b2 -> do
        res1 <- eval -< b1
        res2 <- eval -< b2
        returnA -< res1
