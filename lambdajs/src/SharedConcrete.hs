{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module SharedConcrete where

import           GHC.Generics                                   (Generic)
import           Prelude                                        hiding (break,
                                                                 error, fail,
                                                                 fold, lookup,
                                                                 map, read)
import qualified Prelude
import           SharedInterpreter
import           Syntax
import           Text.Read                                      (readMaybe)

import           Data.Bits                                      (bit, shift)
import           Data.Fixed                                     (mod')
import           Data.Hashable
import           Data.Identifiable
import           Data.List                                      (elemIndex,
                                                                 find,
                                                                 isPrefixOf,
                                                                 sort)
import           Data.List.Split                                (splitOn)
import           Data.Word                                      (Word32)

import           Data.Concrete.Environment
import           Data.Concrete.Error
import           Data.Concrete.Store
import           Data.Order

import           Control.Arrow.Transformer.Concrete.Environment
import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Concrete.Store
import           Control.Arrow.Transformer.State
import           Control.Arrow.Utils                            (map, pi1, pi2)

import           Control.Arrow
import           Control.Arrow.Environment
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Store
import           Control.Category
import           Debug.Trace                                    (trace)


data Value
    = VNumber Double
    | VString String
    | VBool Bool
    | VUndefined
    | VNull
    | VLambda [Ident] Expr (Env Ident Value)
    | VObject [(String, Value)]
    | VRef Location
    deriving (Show, Eq, Generic)
instance Hashable Value
instance Ord (Env Ident Value) where
    (<=) a b = (sort $ Data.Concrete.Environment.toList a) <= (sort $ Data.Concrete.Environment.toList b)

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
deriving instance ArrowState Location ConcreteArr
deriving instance ArrowChoice ConcreteArr
deriving instance Arrow ConcreteArr
deriving instance Category ConcreteArr
deriving instance ArrowRead Location Value x Value ConcreteArr
deriving instance ArrowWrite Location Value ConcreteArr
deriving instance ArrowExcept (Label, Expr) (Label, Value) (Either String Exceptional) ConcreteArr
deriving instance ArrowExcept (Expr, Expr) (Expr, Value) (Either String Exceptional) ConcreteArr

runLJS :: ConcreteArr x y -> [(Ident, Value)] -> [(Location, Value)] -> x -> (Location, (Store Location Value, Error (Either String Exceptional) y))
runLJS (ConcreteArr f) env env2 x = runState (runStore (runEnvironment (runExcept f))) (Location 0, (Data.Concrete.Store.fromList env2, (Data.Concrete.Environment.fromList env, x)))

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
    (OStrSplitStrExp, [(VString subject), (VString delim)]) -> do
        let elems = zip (Prelude.map show [0..]) (Prelude.map VString $ splitOn delim subject)
        returnA -< VObject (elems ++ [("length", VNumber $ fromIntegral $ length elems), ("$proto", VString "Array")])
    (OStrSplitRegExp, _) -> fail -< Left $ "Regex operations not implemented"
    (ORegExpMatch, _) -> fail -< Left $ "Regex operations not implemented"
    (ORegExpQuote, _) -> fail -< Left $ "Regex operations not implemented"
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
        (VNumber _)     -> VString "number"
        (VString _)     -> VString "string"
        (VBool _)       -> VString "boolean"
        (VUndefined)    -> VString "undefined"
        (VNull)         -> VString "null"
        (VLambda _ _ _) -> VString "lambda"
        (VObject _)     -> VString "object"
        (VRef l)        -> VString "location")
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
    (OAbstractEq, [VNull, VUndefined]) -> returnA -< VBool True
    (OAbstractEq, [VUndefined, VNull]) -> returnA -< VBool True
    (OAbstractEq, [a, b]) -> returnA -< VBool $ a == b
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
    (OObjIterHasNext, [(VObject obj), VUndefined]) -> do
        let newObj = filter (\(n, v) -> (head n /= '$')) obj
        returnA -< VBool $ (length newObj) > 0
    (OObjIterHasNext, [(VObject obj), (VNumber i)]) -> do
        let obj2 = drop ((floor i) + 1) obj
        let obj3 = filter (\(n, v) -> (head n /= '$')) obj2
        returnA -< (VBool $ (length obj3) > 0)
    (OObjIterNext, [(VObject obj), VUndefined]) -> do
        let newObj = filter (\(n, v) -> (head n /= '$')) obj
        case elemIndex (head newObj) obj of
            Just n  -> returnA -< (VNumber $ fromIntegral n)
            Nothing -> fail -< Left $ "Error no such element"
    (OObjIterNext, [(VObject obj), (VNumber i)]) -> do
        let obj2 = drop ((floor i) + 1) obj
        let elem = head $ dropWhile (\(n, v) -> (head n == '$')) obj2
        case elemIndex elem obj of
            Just n  -> returnA -< (VNumber $ fromIntegral n)
            Nothing -> fail -< Left $ "Error no such element"
    (OObjIterKey, [(VObject obj), (VNumber i)]) -> do
        returnA -< (VString $ fst $ obj !! floor i)
    (OSurfaceTypeof, [a]) -> returnA -< VString (case a of
        VObject fields -> if elem "$code" (Prelude.map fst fields) then "function" else "object"
        VNull          -> "object"
        VUndefined     -> "undefined"
        VNumber _      -> "number"
        VString _      -> "string"
        VBool _        -> "boolean")
    x -> fail -< Left $ "Unimplemented operator: " ++ (show op) ++ " with args: " ++ (show vals)

fresh :: ArrowState Location c => c () Location
fresh = proc () -> do
    Location s <- Control.Arrow.State.get -< ()
    put -< Location $ s + 1
    returnA -< Location $ s + 1

getField_ :: (ArrowFail (Either String Exceptional) c, ArrowChoice c, ArrowRead Location Value Value Value c) => c (Value, Value) Value
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
                            protoV <- read pi1 Control.Category.id -< (l, VUndefined)
                            getField_ -< (protoV, VString fieldName)
                        -- When proto exists but none of the special semantics apply
                        Just (_, _) -> returnA -< VUndefined
                        -- E-GetField-NotFound
                        Nothing -> returnA -< VUndefined

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
                _ -> fail -< Left "Error: deleteField returned non-object value"
        _ -> do
            env <- getEnv -< ()
            fail -< Left $ "Error: non exhaustive pattern in updateField_ with params: (" ++ (show fields) ++ ") (" ++ (show name) ++ ") (" ++ show value ++ ") " ++ (show env)

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
    lambdaVal = proc (ids, body) -> do
        env <- getEnv -< ()
        returnA -< VLambda ids body env
    objectVal = proc (fields) -> returnA -< VObject fields
    getField f1 = proc (obj, fieldE) -> do
        field <- f1 -< fieldE
        getField_ -< (obj, field)
    updateField f1 = proc (obj, fieldE, val) -> do
        field <- f1 -< fieldE
        updateField_ -< (obj, field, val)
    deleteField f1 = proc (obj, fieldE) -> do
        field <- f1 -< fieldE
        deleteField_ -< (obj, field)
    -- operator/delta function
    evalOp = proc (op, vals) -> evalOp_ -< (op, vals)
    -- environment ops
    lookup = proc id -> do
        v <- Control.Arrow.Environment.lookup pi1 Control.Category.id -< (id, VRef (Location (-1)))
        case v of
            VRef (Location (-1)) -> fail -< Left $ "Error: " ++ (show id) ++ " does not exist"
            _ -> returnA -< v
    apply f1 = proc (lambda, args) -> do
        case lambda of
            VLambda names body closureEnv -> do
                case (length names) == (length args) of
                    False -> fail -< Left $ "Error: applied lambda with less/more params than arguments"
                    True -> do
                        newBindings <- arr $ uncurry zip -< (names, args)
                        bindingEnv <- bindings -< (newBindings, closureEnv)
                        outsideEnv <- getEnv -< ()
                        finalEnv <- bindings -< (Data.Concrete.Environment.toList bindingEnv, outsideEnv)
                        localEnv f1 -< (finalEnv, body)
            _ -> fail -< Left $ "Error: apply on non-lambda value: " ++ (show lambda) ++ " " ++ (show args)
    -- store ops
    set = proc (loc, val) -> do
        case loc of
            VRef l -> do
                write -< (l, val)
                returnA -< ()
            _ -> fail -< Left $ "Error: ESetRef lhs must be location, is: " ++ (show loc)
    new = proc (val) -> do
        loc <- fresh -< ()
        set -< (VRef loc, val)
        returnA -< VRef loc
    get = proc (loc) -> do
        case loc of
            VRef l -> do
                val <- read pi1 Control.Category.id -< (l, VUndefined)
                returnA -< val
            _ -> fail -< Left $ "Error: EDeref lhs must be location, is: " ++ (show loc)
    -- control flow
    if_ f1 f2 = proc (cond, thenBranch, elseBranch) -> do
        case cond of
            VBool True -> do
                f1 -< thenBranch
            VBool False -> do
                f2 -< elseBranch
            _ -> fail -< Left $ (show cond)
    while_ f1 f2 = proc (cond, body) -> do
        condV <- f1 -< cond
        case condV of
            VBool True  -> f2 -< (ESeq body (EWhile cond body))
            VBool False -> returnA -< VUndefined
    label f1 = proc (l, e) -> do
        (l, res) <- tryCatch (second f1) (proc ((label, _), err) -> case err of
            Left s -> fail -< Left s
            Right (Break l1 v) -> case l1 == label of
                True  -> returnA -< (label, v)
                False -> fail -< (Right $ Break l1 v)
            Right (Thrown v) -> fail -< (Right $ Thrown v)) -< (l, e)
        returnA -< res
    break = proc (l, v) -> do
        fail -< Right (Break l v)
    throw = proc v -> do
        fail -< Right (Thrown v)
    catch f1 = proc (try, catch) -> do
        (c, res) <- tryCatch (second f1) (proc ((catch, _), err) -> case err of
            Left s -> fail -< Left s
            Right (Break l1 v) -> fail -< Right $ Break l1 v
            Right (Thrown v) -> case catch of
                ELambda [x] body -> do
                    scope <- getEnv -< ()
                    env' <- extendEnv -< (x, v, scope)
                    res <- localEnv f1 -< (env', body)
                    returnA -< (catch, res)
                _ -> fail -< Left "Error: Catch block must be of type ELambda") -< (catch, try)
        returnA -< res
    error = proc s -> fail -< Left $ "Error: aborted with message: " ++ s
