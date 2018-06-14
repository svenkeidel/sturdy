{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module SharedConcrete where

import           GHC.Generics                                   (Generic)
import           Prelude                                        hiding (break,
                                                                 error, lookup,
                                                                 read)
import qualified Prelude
import           SharedInterpreter
import           Syntax
import           Text.Read                                      (readMaybe)

import           Data.Bits                                      (bit, shift)
import           Data.Fixed                                     (mod')
import           Data.Hashable
import           Data.Identifiable
import           Data.List                                      (find,
                                                                 isPrefixOf)
import           Data.Word                                      (Word32)

import           Data.Concrete.Environment
import           Data.Concrete.Error
import           Data.Concrete.Store
import           Data.Order

import           Control.Arrow.Transformer.Concrete.Environment
import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Concrete.Store
import           Control.Arrow.Transformer.State
import           Control.Arrow.Utils                            (mapA, pi1, pi2)

import           Control.Arrow
import           Control.Arrow.Environment
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Store
import           Control.Category
import           Debug.Trace


data Value
    = VNumber Double
    | VString String
    | VBool Bool
    | VUndefined
    | VNull
    | VLambda [Ident] Expr
    | VObject [(String, Value)]
    | VRef Location
    deriving (Show, Eq, Generic)
instance Hashable Value
deriving instance Ord Value

data Exceptional
    = Break Label Value
    | Thrown Value
    deriving (Show, Eq, Generic)
instance Hashable Exceptional
deriving instance Ord Exceptional

newtype ConcreteArr x y = ConcreteArr
    (Except
        (Either String Exceptional)
        (Environment Ident Value
            (StoreArrow Location Value
                (State Location (->)))) x y)

deriving instance ArrowFail (Either String Exceptional) ConcreteArr
deriving instance ArrowEnv Ident Value (Env Ident Value) ConcreteArr

instance (Show Location, Identifiable Value, ArrowChoice c) => ArrowStore Location Value lab (StoreArrow Location Value c) where
  read =
    StoreArrow $ State $ proc (s,(var,_)) -> case Data.Concrete.Store.lookup var s of
      Just v  -> returnA -< (s,v)
      Nothing -> returnA -< (s, VUndefined)
  write = StoreArrow (State (arr (\(s,(x,v,_)) -> (Data.Concrete.Store.insert x v s,()))))
instance (Show Ident, Identifiable Ident, ArrowChoice c) => ArrowEnv Ident Value (Env Ident Value) (Environment Ident Value c) where
  lookup = proc x -> do
    env <- getEnv -< ()
    case Data.Concrete.Environment.lookup x env of
      Just y  -> returnA -< y
      Nothing -> returnA -< VRef $ Location (-1)
  getEnv = Environment askA
  extendEnv = arr $ \(x,y,env) -> Data.Concrete.Environment.insert x y env
  localEnv (Environment f) = Environment (localA f)

deriving instance ArrowStore Location Value () ConcreteArr
deriving instance ArrowState Location ConcreteArr
deriving instance ArrowChoice ConcreteArr
deriving instance Arrow ConcreteArr
deriving instance Category ConcreteArr


deriving instance ArrowExcept (Label, Expr) (Label, Value) (Either String Exceptional) ConcreteArr
deriving instance ArrowExcept (Expr, Expr) (Expr, Value) (Either String Exceptional) ConcreteArr

runLJS :: ConcreteArr x y -> [(Ident, Value)] -> [(Location, Value)] -> x -> (Location, (Store Location Value, Error (Either String Exceptional) y))
runLJS (ConcreteArr f) env env2 x = runState (runStore (runEnvironment (runExcept f))) (Location 0, (Data.Concrete.Store.fromList env2, (env, x)))

runConcrete :: [(Ident, Value)] -> [(Location, Value)] -> Expr -> (Store Location Value, Error String Value)
runConcrete env st exp =
    case runLJS eval env st exp of
        (_, (st, Fail (Left e))) -> (st, Fail e)
        (_, (st, Fail (Right e))) -> (st, Fail $ "Error: Uncaught throws or label break: " ++ (show e))
        (_, (st, Success res)) -> (st, Success res)

evalOp_ :: (ArrowFail (Either String Exceptional) c, ArrowChoice c) => c (Op, [Value]) Value
evalOp_ = proc (op, vals) -> case (op, vals) of
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
        (VNumber _)  -> VBool True
        (VString _)  -> VBool True
        (VBool _)    -> VBool True
        (VNull)      -> VBool True
        (VUndefined) -> VBool True
        _            -> VBool False)
    -- primToNum operator
    -- #todo object conversions -> valueOf call
    (OPrimToNum, [a]) -> returnA -< (case a of
        (VNumber a)  -> VNumber a
        (VString s)  -> VNumber $ (case (readMaybe s :: Maybe Double) of
            Just num -> num
            Nothing  -> 0/0)
        (VBool b)    -> if b then VNumber 1.0 else VNumber 0.0
        (VNull)      -> VNumber 0
        (VUndefined) -> VNumber (0/0))
    -- primToStr operator
    (OPrimToStr, [a]) -> returnA -< (case a of
        (VNumber a)  -> if (fromInteger $ floor a) == a then (VString $ show $ floor a) else (VString $ show a)
        (VString s)  -> VString s
        (VBool b)    -> VString $ if b then "true" else "false"
        (VNull)      -> VString "null"
        (VUndefined) -> VString "undefined"
        (VObject _)  -> VString "object")
    -- primToBool operator
    (OPrimToBool, [a]) -> returnA -< (case a of
        (VNumber a)  -> VBool $ (a /= 0.0) && (not (isNaN a))
        (VString s)  -> VBool $ not $ s == ""
        (VBool b)    -> VBool b
        (VNull)      -> VBool False
        (VUndefined) -> VBool False
        (VObject _)  -> VBool True)
    -- typeOf operator
    (OTypeof, [a]) -> returnA -< (case a of
        (VNumber _)   -> VString "number"
        (VString _)   -> VString "string"
        (VBool _)     -> VString "boolean"
        (VUndefined)  -> VString "undefined"
        (VNull)       -> VString "object"
        (VLambda _ _) -> VString "lambda"
        (VObject _)   -> VString "object"
        (VRef l)      -> VString "location")
    -- equality operators
    (OStrictEq, [a, b]) -> returnA -< VBool $ a == b
    (OAbstractEq, [(VNumber a), (VString b)]) -> do
        res <- evalOp_ -< (OPrimToNum, [VString b])
        returnA -< VBool $ (VNumber a) == res
    (OAbstractEq, [(VString a), (VNumber b)]) -> do
        res <- evalOp_ -< (OPrimToNum, [VString a])
        returnA -< VBool $ (VNumber b) == res
    (OAbstractEq, [(VBool a), (VNumber b)]) -> do
        res <- evalOp_ -< (OPrimToNum, [VBool a])
        returnA -< VBool $ (VNumber b) == res
    (OAbstractEq, [(VNumber a), (VBool b)]) -> do
        res <- evalOp_ -< (OPrimToNum, [VBool b])
        returnA -< VBool $ (VNumber a) == res
    (OAbstractEq, [(VNumber a), (VNumber b)]) -> returnA -< VBool $ a == b
    (OAbstractEq, [VNull, VUndefined]) -> returnA -< VBool True
    (OAbstractEq, [VUndefined, VNull]) -> returnA -< VBool True
    (OAbstractEq, [VString a, VString b]) -> returnA -< VBool $ a == b
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
    (OObjCanDelete, [(VObject fields), (VString field)]) ->
        returnA -< VBool $ (length field) > 0 && (not $ head field == '$')
    (OSurfaceTypeof, [a]) -> returnA -< VString (case a of
        VObject fields -> if elem "$code" (map fst fields) then "function" else "object"
        VNull          -> "object"
        VUndefined     -> "undefined"
        VNumber _      -> "number"
        VString _      -> "string"
        VBool _        -> "boolean")
    x -> failA -< Left $ "Unimplemented operator: " ++ (show op) ++ " with args: " ++ (show vals)

fresh :: ArrowState Location c => c () Location
fresh = proc () -> do
    Location s <- getA -< ()
    putA -< Location $ s + 1
    returnA -< Location $ s + 1

getField_ :: (ArrowFail (Either String Exceptional) c, ArrowChoice c, ArrowStore Location Value () c) => c (Value, Value) Value
getField_ = proc (VObject fields, VString fieldName) -> do
    let fieldV = find (\(fn, fv) -> fieldName == fn) fields in
        case fieldV of
            -- E-GetField
            Just (n, v) -> returnA -< v
            Nothing ->
                let protoFieldV = find (\(fn, fv) -> fn == "$proto") fields in
                    case protoFieldV of
                        -- E-GetField-Proto-Null
                        Just (pn, VNull) -> returnA -< VUndefined
                        -- E-GetField-Proto
                        Just (pn, VRef l) -> do
                            protoV <- read -< (l, ())
                            getField_ -< (protoV, VString fieldName)
                        -- E-GetField-NotFound
                        Nothing -> returnA -< VUndefined
                        _ -> failA -< Left $ "Error: $proto field must be null or reference if it exists"

updateField_ :: (ArrowFail (Either String Exceptional) e, ArrowChoice e, ArrowEnv Ident Value (Env Ident Value) e) => e (Value, Value, Value) Value
updateField_ = proc (fields, name, value) -> do
    case (fields, name) of
        (VObject fields, VString name) -> do
            -- remove field from obj
            filtered <- deleteField_ -< (VObject fields, VString name)
            case filtered of
                VObject obj -> do
                    -- add field with new value to obj
                    newFields <- arr (\(fs, n, v) -> (n, v) : fs) -< (obj, name, value)
                    returnA -< VObject newFields
                _ -> failA -< Left "Error: deleteField returned non-object value"
        _ -> do
            env <- getEnv -< ()
            failA -< Left $ "Error: non exhaustive pattern in updateField_ with params: (" ++ (show fields) ++ ") (" ++ (show name) ++ ") (" ++ show value ++ ") " ++ (show env)

deleteField_ :: ArrowFail (Either String Exceptional) e => e (Value, Value) Value
deleteField_ = proc (VObject obj, VString field) -> do
    filtered <- arr $ (\(fs, n) -> (filter (\(fn, _) -> fn /= n) fs)) -< (obj, field)
    returnA -< VObject filtered

instance {-# OVERLAPS #-} AbstractValue Value ConcreteArr where
    -- values
    numVal = proc n -> returnA -< VNumber n
    boolVal = proc b -> returnA -< VBool b
    stringVal = proc s -> returnA -< VString s
    undefVal = proc () -> returnA -< VUndefined
    nullVal = proc () -> returnA -< VNull
    lambdaVal = proc (ids, body) -> returnA -< VLambda ids body
    objectVal = proc (fields) -> returnA -< VObject fields
    getField = proc (obj, field) -> getField_ -< (obj, field)
    updateField = proc (obj, field, val) -> updateField_ -< (obj, field, val)
    deleteField = proc (obj, field) -> deleteField_ -< (obj, field)
    -- operator/delta function
    evalOp = proc (op, vals) -> evalOp_ -< (op, vals)
    -- environment ops
    lookup = proc id -> do
        v <- Control.Arrow.Environment.lookup -< id
        case v of
            VRef (Location (-1)) -> failA -< Left $ "Error: " ++ (show id) ++ " does not exist"
            _ -> returnA -< v
    apply f1 = proc (lambda, args) -> do
        case lambda of
            VLambda names body -> do
                case (length names) == (length args) of
                    False -> failA -< Left $ "Error: applied lambda with less/more params than arguments"
                    True -> do
                        forEnv <- arr $ uncurry zip -< (names, args)
                        scope <- getEnv -< ()
                        env' <- bindings -< (forEnv, scope)
                        localEnv f1 -< (env', body)
            _ -> failA -< Left $ "Error: apply on non-lambda value: " ++ (show lambda) ++ " " ++ (show args)
    -- store ops
    set = proc (loc, val) -> do
        case loc of
            VRef l -> do
                write -< (l, val, ())
                returnA -< ()
            _ -> failA -< Left $ "Error: ESetRef lhs must be location, is: " ++ (show loc)
    new = proc (val) -> do
        loc <- fresh -< ()
        set -< (VRef loc, val)
        returnA -< VRef loc
    get = proc (loc) -> do
        case loc of
            VRef l -> do
                val <- read -< (l, ())
                returnA -< val
            _ -> failA -< Left $ "Error: EDeref lhs must be location, is: " ++ (show loc)
    -- control flow
    if_ f1 f2 = proc (cond, thenBranch, elseBranch) -> do
        case cond of
            VBool True -> do
                f1 -< thenBranch
            VBool False -> do
                f2 -< elseBranch
            _ -> failA -< Left $ (show cond)
    label f1 = proc (l, e) -> do
        (l, res) <- tryCatchA (second f1) (proc ((label, _), err) -> case err of
            Left s -> failA -< Left s
            Right (Break l1 v) -> case l1 == label of
                True  -> returnA -< (label, v)
                False -> failA -< (Right $ Break l1 v)
            Right (Thrown v) -> failA -< (Right $ Thrown v)) -< (l, e)
        returnA -< res
    break = proc (l, v) -> do
        failA -< Right (Break l v)
    throw = proc v -> do
        failA -< Right (Thrown v)
    catch f1 = proc (try, catch) -> do
        (c, res) <- tryCatchA (second f1) (proc ((catch, _), err) -> case err of
            Left s -> failA -< Left s
            Right (Break l1 v) -> failA -< Right $ Break l1 v
            Right (Thrown v) -> case catch of
                ELambda [x] body -> do
                    scope <- getEnv -< ()
                    env' <- extendEnv -< (x, v, scope)
                    res <- localEnv f1 -< (env', body)
                    returnA -< (catch, res)
                _ -> failA -< Left "Error: Catch block must be of type ELambda") -< (catch, try)
        returnA -< res
    error = proc s -> failA -< Left $ "Error: aborted with message: " ++ s
