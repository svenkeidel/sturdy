{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module SharedInterpreter where

import Prelude hiding(lookup, break, read, error)
import qualified Prelude
import Syntax

import Data.Concrete.Error
import Data.Concrete.Store
import Data.Concrete.Environment
import Control.Category
import Control.Arrow
import Control.Arrow.Transformer.Concrete.Except
import Control.Arrow.Transformer.Concrete.Environment
import Control.Arrow.Transformer.Concrete.Store
import Control.Arrow.Transformer.State
import Control.Arrow.Fail
import Control.Arrow.Try
import Control.Arrow.Const
import Control.Arrow.Environment
import Control.Arrow.State
import Control.Arrow.Store
import Control.Arrow.Reader
import Control.Arrow.Utils (foldA, mapA, pi2, pi1)
import Control.Arrow.TryCatch
import Data.Identifiable
import Data.Fixed (mod')
import Data.List (isPrefixOf, find)
import Data.Bits (shift, bit)
import Data.Word (Word32)

import Data.Map (Map, findWithDefault)
import qualified Data.Map as Map

class Arrow c => AbstractValue v c | c -> v where
    -- values
    numVal :: c Double v
    boolVal :: c Bool v
    stringVal :: c String v
    undefVal :: c () v
    nullVal :: c () v
    lambdaVal :: c ([Ident], Expr) v
    objectVal :: c [(Ident, v)] v
    getField :: c (v, v) v
    updateField :: c (v, v, v) v
    deleteField :: c (v, v) v
    -- operator/delta function
    evalOp :: c (Op, [v]) v
    -- environment ops
    lookup :: c Ident v
    apply :: c ([(Ident, v)], Expr) v
    -- store ops
    set :: c (v, v) ()
    new :: c v v
    get :: c v v
    -- control flow
    if_ :: c (v, Expr, Expr) v
    label :: c (Label, Expr) v
    break :: c (Label, v) v
    catch :: c (Expr, Expr) v
    throw :: c v v
    -- exceptional
    error :: c () v

eval :: (ArrowChoice c, AbstractValue v c) => c Expr v
eval = proc e -> case e of
    ENumber d -> numVal -< d
    EString s -> stringVal -< s
    EBool b -> boolVal -< b
    EUndefined -> undefVal -< ()
    ENull -> nullVal -< ()
    ELambda ids exp -> lambdaVal -< (ids, exp)
    EObject fields -> do
        vals <- (mapA $ second eval) -< fields
        objectVal -< vals
    EId id -> SharedInterpreter.lookup -< id
    EOp op exps -> do
        vals <- (mapA eval) -< exps
        evalOp -< (op, vals)
    EApp (ELambda params body) args -> do
        vals <- mapA eval -< args
        pairs <- arr $ uncurry zip -< (params, vals)
        apply -< (pairs, body)
    ELet argsE body -> do
        eval -< EApp (ELambda (map fst argsE) body) (map snd argsE)
    ESetRef locE valE -> do
        loc <- eval -< locE
        val <- eval -< valE
        set -< (loc, val)
        undefVal -< ()
    ERef valE -> do
        val <- eval -< valE
        new -< val
    EDeref locE -> do
        loc <- eval -< locE
        get -< loc
    EGetField objE fieldE -> do
        obj <- eval -< objE
        field <- eval -< fieldE
        getField -< (obj, field)
    EUpdateField objE fieldE valE -> do
        obj <- eval -< objE
        field <- eval -< fieldE
        val <- eval -< valE
        updateField -< (obj, field, val)
    EDeleteField objE fieldE -> do
        obj <- eval -< objE
        field <- eval -< fieldE
        deleteField -< (obj, field)
    ESeq one two -> do
        eval -< one
        eval -< two
    EIf condE thenE elseE -> do
        cond <- eval -< condE
        if_ -< (cond, thenE, elseE)
    EWhile cond body -> do
        eval -< EIf cond (ESeq body (EWhile cond body)) (EUndefined)
    ELabel l e -> do
        label -< (l, e)
    EBreak l e -> do
        val <- eval -< e
        break -< (l, val)
    EThrow e -> do
        val <- eval -< e
        throw -< val
    ECatch try catchE -> do
        catch -< (try, catchE)
    EFinally e1 e2 -> do
        res <- eval -< e1
        eval -< e2
        returnA -< res
    EEval -> error -< ()

newtype LJSArrow x y = LJSArrow (Except (Either String Exceptional) (Environment Ident Location (StoreArrow Location Value (State Location (->)))) x y)
    deriving (ArrowChoice,Arrow,Category)
deriving instance ArrowFail (Either String Exceptional) LJSArrow
deriving instance ArrowEnv Ident Location (Env Ident Location) LJSArrow

instance (Show Location, Identifiable Value, ArrowChoice c) => ArrowStore Location Value lab (StoreArrow Location Value c) where
  read =
    StoreArrow $ State $ proc (s,(var,_)) -> case Data.Concrete.Store.lookup var s of
      Success v -> returnA -< (s,v)
      Fail _ -> returnA -< (s, VUndefined)
  write = StoreArrow (State (arr (\(s,(x,v,_)) -> (Data.Concrete.Store.insert x v s,()))))
instance (Show Ident, Identifiable Ident, ArrowChoice c) => ArrowEnv Ident Location (Env Ident Location) (Environment Ident Location c) where
  lookup = proc x -> do
    env <- getEnv -< ()
    case Data.Concrete.Environment.lookup x env of
      Success y -> returnA -< y
      Fail _ -> returnA -< Location 0
  getEnv = Environment askA
  extendEnv = arr $ \(x,y,env) -> Data.Concrete.Environment.insert x y env
  localEnv (Environment f) = Environment (localA f)

deriving instance ArrowStore Location Value () LJSArrow
deriving instance ArrowState Location LJSArrow

instance ArrowTryCatch (Either String Exceptional) (Label, Expr) (Label, Value) LJSArrow where
    tryCatchA (LJSArrow f) (LJSArrow g) = LJSArrow $ tryCatchA f g
instance ArrowTryCatch (Either String Exceptional) (Expr, Expr) (Expr, Value) LJSArrow where
    tryCatchA (LJSArrow f) (LJSArrow g) = LJSArrow $ tryCatchA f g

runLJS :: LJSArrow x y -> [(Ident, Location)] -> [(Location, Value)] -> x -> (Location, (Store Location Value, Error (Either String Exceptional) y))
runLJS (LJSArrow f) env env2 x = runState (runStore (runEnvironment (runExcept f))) (Location 0, (Data.Concrete.Store.fromList env2, (env, x)))

runConcrete :: [(Ident, Location)] -> [(Location, Value)] -> Expr -> (Store Location Value, Error String Value)
runConcrete env st exp = case runLJS eval env st exp of
    (_, (st, Fail (Left e))) -> (st, Fail e)
    (_, (st, Fail (Right _))) -> (st, Fail "Error: Uncaught throws or label break")
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

fresh :: ArrowState Location c => c () Location 
fresh = proc () -> do
    Location s <- getA -< ()
    putA -< Location $ s + 1
    returnA -< Location $ s + 1

getField_ :: (ArrowFail (Either String Exceptional) c, ArrowChoice c) => c (Value, Value) Value
getField_ = proc (VObject fields, VString fieldName) -> 
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
                            res <- getField_ -< (pv, VString fieldName)
                            returnA -< res
                        -- E-GetField-NotFound
                        Nothing -> returnA -< VUndefined

updateField_ :: (ArrowFail (Either String Exceptional) e, ArrowChoice e) => e (Value, Value, Value) Value
updateField_ = proc (VObject fields, VString name, value) -> do
    -- remove field from obj
    filtered <- deleteField_ -< (VObject fields, VString name)
    case filtered of
        VObject obj -> do
            -- add field with new value to obj
            newFields <- arr (\(fs, n, v) -> (n, v) : fs) -< (obj, name, value) 
            returnA -< VObject newFields
        _ -> failA -< Left "Error: deleteField returned non-object value"

deleteField_ :: ArrowFail (Either String Exceptional) e => e (Value, Value) Value
deleteField_ = proc (VObject fields, VString name) -> do
    filtered <- arr (\(n, fs) -> filter (\(fn, _) -> fn /= n) fs) -< (name, fields) 
    returnA -< VObject filtered

instance {-# OVERLAPS #-} AbstractValue Value LJSArrow where
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
        loc <- Control.Arrow.Environment.lookup -< id
        returnA -< VRef loc
    apply = proc ((pairs), body) -> do
        -- generate locations for arguments equal to length of pairs 
        locations <- mapA ((arr $ const ()) >>> fresh) -< pairs

        vals <- mapA $ pi2 -< pairs
        ids <- mapA $ pi1 -< pairs

        forStore <- arr $ uncurry zip -< (locations, vals) 
        mapA set -< map (\(a, b) -> (VRef a, b)) forStore

        forEnv <- arr $ uncurry zip -< (ids, locations) 
        scope <- getEnv -< ()
        env' <- bindings -< (forEnv, scope)
        localEnv eval -< (env', body)
    -- store ops
    set = proc (loc, val) -> do
        case loc of
            VRef l -> do
                write -< (l, val, ())
                returnA -< ()
            _ -> failA -< Left "Error: ESetRef lhs must be location"
    new = proc (val) -> do 
        loc <- fresh -< ()
        set -< (VRef loc, val)
        returnA -< VRef loc
    get = proc (loc) -> do
        case loc of
            VRef l -> do
                val <- read -< (l, ())
                returnA -< val
            _ -> failA -< Left "Error: EDeref lhs must be location"
    -- control flow
    if_ = proc (cond, thenBranch, elseBranch) -> do
        case cond of
            VBool True -> do
                eval -< thenBranch
            VBool False -> do
                eval -< elseBranch
    label = proc (l, e) -> do
        (l, res) <- tryCatchA (second eval) (proc ((label, _), err) -> case err of
            Left s -> failA -< Left s
            Right (Break l1 v) -> case l1 == label of
                True -> returnA -< (label, v)
                False -> failA -< (Right $ Break l1 v)
            Right (Thrown v) -> failA -< (Right $ Thrown v)) -< (l, e)
        returnA -< res
    break = proc (l, v) -> do
        failA -< Right (Break l v)
    throw = proc v -> do
        failA -< Right (Thrown v)
    catch = proc (try, catch) -> do
        (c, res) <- tryCatchA (second eval) (proc ((catch, _), err) -> case err of
            Left s -> failA -< Left s
            Right (Break l1 v) -> failA -< Right $ Break l1 v
            Right (Thrown v) -> case catch of
                ELambda [x] body -> do
                    scope <- getEnv -< ()
                    loc <- fresh -< ()
                    env' <- extendEnv -< (x, loc, scope)
                    write -< (loc, v, ())
                    res <- localEnv eval -< (env', body)
                    returnA -< (catch, res)
                _ -> failA -< Left "Error: Catch block must be of type ELambda") -< (catch, try)
        returnA -< res
    error = proc () -> failA -< Left "Error: aborted"

newtype TypeArr x y = TypeArr (Except String (Environment Ident Location (StoreArrow Location Type (State Location (->)))) x y)
    deriving (ArrowChoice,Arrow,Category)
deriving instance ArrowFail String TypeArr
deriving instance ArrowEnv Ident Location (Env Ident Location) TypeArr

instance (Show Location, Identifiable Type, ArrowChoice c) => ArrowStore Location Type lab (StoreArrow Location Type c) where
  read =
    StoreArrow $ State $ proc (s,(var,_)) -> case Data.Concrete.Store.lookup var s of
      Success v -> returnA -< (s,v)
      Fail _ -> returnA -< (s, TUndefined)
  write = StoreArrow (State (arr (\(s,(x,v,_)) -> (Data.Concrete.Store.insert x v s,()))))
deriving instance ArrowStore Location Type () TypeArr
deriving instance ArrowState Location TypeArr

runType :: TypeArr x y -> [(Ident, Location)] -> [(Location, Type)] -> x -> (Location, (Store Location Type, Error String y))
runType (TypeArr f) env env2 x = runState (runStore (runEnvironment (runExcept f))) (Location 0, (Data.Concrete.Store.fromList env2, (env, x)))

runAbstract :: [(Ident, Location)] -> [(Location, Type)] -> Expr -> (Store Location Type, Error String Type)
runAbstract env st exp = case runType eval env st exp of
    (l, (st, Fail e)) -> (st, Fail e)
    (l, (st, Success res)) -> (st, Success res)

typeEvalOp_ :: (ArrowFail String c, ArrowChoice c) => c (Op, [Type]) Type
typeEvalOp_ = proc (op, vals) -> case (op, vals) of
    -- number operators
    (ONumPlus, [TNumber, TNumber]) -> returnA -< TNumber
    (OMul, [TNumber, TNumber]) -> returnA -< TNumber
    (ODiv, [TNumber, TNumber]) -> returnA -< TNumber
    (OMod, [TNumber, TNumber]) -> returnA -< TNumber
    (OSub, [TNumber, TNumber]) -> returnA -< TNumber
    (OLt, [TNumber, TNumber]) -> returnA -< TBool
    (OToInteger, [TNumber]) -> returnA -< TNumber
    (OToInt32, [TNumber]) -> returnA -< TNumber
    (OToUInt32, [TNumber]) -> returnA -< TNumber
    -- shift operators
    (OLShift, [TNumber, TNumber]) -> returnA -< TNumber
    (OSpRShift, [TNumber, TNumber]) -> returnA -< TNumber
    (OZfRShift, [TNumber, TNumber]) -> returnA -< TNumber
    -- string operators
    (OStrPlus, [TString, TString]) -> returnA -< TString
    (OStrLt, [TString, TString]) -> returnA -< TBool
    (OStrLen, [TString]) -> returnA -< TNumber
    (OStrLen, [TString, TString]) -> returnA -< TBool
    -- boolean operators
    (OBAnd, [TBool, TBool]) -> returnA -< TBool
    (OBOr, [TBool, TBool]) -> returnA -< TBool
    (OBXOr, [TBool, TBool]) -> returnA -< TBool
    (OBNot, [TBool]) -> returnA -< TBool
    -- isPrimitive operator
    (OIsPrim, [_]) -> returnA -< TBool
    -- primToNum operator
    -- #todo object conversions -> valueOf call
    (OPrimToNum, [_]) -> returnA -< TNumber
    -- primToStr operator
    (OPrimToStr, [_]) -> returnA -< TString
    -- primToBool operator
    (OPrimToBool, [_]) -> returnA -< TBool
    -- typeOf operator
    (OTypeof, [_]) -> returnA -< TString 
    -- equality operators
    (OStrictEq, [a, b]) -> returnA -< TBool
    (OAbstractEq, [TNumber, TString]) -> returnA -< TBool
    (OAbstractEq, [TString, TNumber]) -> returnA -< TBool
    (OAbstractEq, [TBool, TNumber]) -> returnA -< TBool
    (OAbstractEq, [TNumber, TBool]) -> returnA -< TBool
    (OAbstractEq, [TNumber, TNumber]) -> returnA -< TBool
    (OAbstractEq, [TNull, TUndefined]) -> returnA -< TBool
    (OAbstractEq, [TUndefined, TNull]) -> returnA -< TBool
    -- math operators
    (OMathExp, [TNumber]) -> returnA -< TNumber
    (OMathLog, [TNumber]) -> returnA -< TNumber
    (OMathCos, [TNumber]) -> returnA -< TNumber
    (OMathSin, [TNumber]) -> returnA -< TNumber
    (OMathAbs, [TNumber]) -> returnA -< TNumber
    (OMathPow, [TNumber, TNumber]) -> returnA -< TNumber
    -- object operators
    (OHasOwnProp, [(TObject _), TString]) -> returnA -< TBool

instance {-# OVERLAPS #-} AbstractValue Type TypeArr where
    -- values
    numVal = proc _ -> returnA -< TNumber
    boolVal = proc _ -> returnA -< TBool
    stringVal = proc _ -> returnA -< TString
    undefVal = proc () -> returnA -< TUndefined
    nullVal = proc () -> returnA -< TNull
    lambdaVal = proc (ids, body) -> do
        bodyT <- eval -< body
        returnA -< TLambda ids bodyT
    objectVal = proc (fields) -> do
        returnA -< TObject fields
    getField = proc (_, _) -> returnA -< TTop
    updateField = proc (_, _, _) -> returnA -< TTop
    deleteField = proc (_, _) -> returnA -< TTop
    -- operator/delta function
    evalOp = proc (op, vals) -> typeEvalOp_ -< (op, vals)
    -- environment ops
    lookup = proc id -> do
        loc <- Control.Arrow.Environment.lookup -< id
        returnA -< TRef loc
    apply = proc ((pairs), body) -> do
        -- generate locations for arguments equal to length of pairs 
        locations <- mapA ((arr $ const ()) >>> fresh) -< pairs

        vals <- mapA $ pi2 -< pairs
        ids <- mapA $ pi1 -< pairs

        forStore <- arr $ uncurry zip -< (locations, vals) 
        mapA set -< map (\(a, b) -> (TRef a, b)) forStore

        forEnv <- arr $ uncurry zip -< (ids, locations) 
        scope <- getEnv -< ()
        env' <- bindings -< (forEnv, scope)
        localEnv eval -< (env', body)
    -- store ops
    set = proc (loc, val) -> do
        case loc of
            TRef l -> do
                write -< (l, val, ())
                returnA -< ()
            _ -> failA -< "Error: ESetRef lhs must be location"
    new = proc (val) -> do 
        loc <- fresh -< ()
        set -< (TRef loc, val)
        returnA -< TRef loc
    get = proc (loc) -> do
        case loc of
            TRef l -> do
                val <- read -< (l, ())
                returnA -< val
            _ -> failA -< "Error: EDeref lhs must be location"
    -- control flow
    if_ = proc (cond, thenBranch, elseBranch) -> do
        case cond of
            TBool -> do
                thenT <- eval -< thenBranch
                elseT <- eval -< elseBranch
                case thenT == elseT of
                    True -> returnA -< thenT
                    False -> failA -< "Error: Branches of conditional must be of equal type"
            _ -> failA -< "Error: Conditional must be of type bool"
    label = proc (l, e) -> do
        eT <- eval -< e
        case eT of
            TBreak l1 t -> case l == l1 of
                True -> returnA -< t
                False -> failA -< "Error: Expression within label must be of type break to that label"
            _ -> failA -< "Error: Expression within label must be of type break to that label"
    break = proc (l, t) -> do
        returnA -< TBreak l t
    throw = proc t -> do
        returnA -< TThrown t
    catch = proc (try, catch) -> do
        tryT <- eval -< try
        case tryT of
            TThrown t -> returnA -< t
            _ -> failA -< "Error: Expression within try must be of type thrown"
    error = proc () -> failA -< "Error: aborted"
