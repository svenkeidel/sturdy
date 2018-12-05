{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module TypeSemantics where

import           Derivations                                           ()
import           GHC.Generics                                          ()
import           Prelude                                               hiding
                                                                        (break,
                                                                        error,
                                                                        fail,
                                                                        lookup,
                                                                        map,
                                                                        read)
import qualified Prelude
import           SharedInterpreter
import           Syntax

import           Data.Hashable                                         ()
import           Data.Identifiable                                     ()

import           Control.Arrow.Transformer.Abstract.BoundedEnvironment as BE
import           Control.Arrow.Transformer.Abstract.Fixpoint
import           Control.Arrow.Transformer.Abstract.HandleExcept
import           Control.Arrow.Transformer.Abstract.Store              as S
import           Control.Arrow.Transformer.State
import           Control.Arrow.Utils                                   (map,
                                                                        pi1)
import           Data.Abstract.DiscretePowerset                        as P
import qualified Data.Abstract.FiniteMap                               as M
import           Data.Abstract.HandleError
import           Data.Abstract.PreciseStore                            as PS
import           Data.Abstract.StackWidening                           as SW
import           Data.Abstract.Store
import           Data.Abstract.Terminating
import           Data.Abstract.Widening                                as W
import           Data.List                                             (find)
import           Data.Order

import           Control.Arrow
import           Control.Arrow.Environment
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.State
import           Control.Arrow.Store
import           Control.Category

newtype TypeArr s x y = TypeArr
    (Fix Expr Type'
        (Except String
            (BE.Environment Ident Location Type'
                (StoreArrow Location Type'
                    (State Location
                        (Fixpoint s () () (->)))))) x y)

deriving instance ArrowFail String (TypeArr s)
deriving instance ArrowEnv Ident Type' (M.Map Ident Location Type') (TypeArr s)
deriving instance Category (TypeArr s)
deriving instance Arrow (TypeArr s)
deriving instance ArrowChoice (TypeArr s)
deriving instance ArrowRead Location Type' x Type' (TypeArr s)
deriving instance ArrowWrite Location Type' (TypeArr s)
deriving instance ArrowState Location (TypeArr s)
deriving instance ArrowFix Expr Type' (TypeArr s)

runType :: TypeArr (SW.Unit) x y -> [(Ident, Type)] -> x -> Terminating (Location, (PS.Store Location Type', Error String y))
runType (TypeArr f) env x =
    runFix' SW.finite W.finite
        (runState
            (runStore
                (BE.runEnvironment fresh2
                (runExcept f))))
    (Location 0, (st', (env', x)))
    where
        env' = Prelude.map (\(a, b) -> (a, P.singleton b)) env
        st' = PS.empty

typeEvalBinOp_ :: (ArrowChoice c) => c (Op, Type, Type) Type
typeEvalBinOp_ = proc (op, v1, v2) -> (arr $ \(op, v1, v2) -> case (op, v1, v2) of
    -- number operators
    (ONumPlus, TNumber, TNumber)        -> TNumber
    (OMul, TNumber, TNumber)            -> TNumber
    (ODiv, TNumber, TNumber)            -> TNumber
    (OMod, TNumber, TNumber)            -> TNumber
    (OSub, TNumber, TNumber)            -> TNumber
    (OLt, TNumber, TNumber)             -> TBool
    -- shift operators
    (OLShift, TNumber, TNumber)         -> TNumber
    (OSpRShift, TNumber, TNumber)       -> TNumber
    (OZfRShift, TNumber, TNumber)       -> TNumber
    -- string operators
    (OStrPlus, TString, TString)        -> TString
    (OStrLt, TString, TString)          -> TBool
    -- boolean operators
    (OBAnd, TBool, TBool)               -> TBool
    (OBOr, TBool, TBool)                -> TBool
    (OBXOr, TBool, TBool)               -> TBool
    -- equality operators
    (OStrictEq, _, _)                   -> TBool
    (OAbstractEq, TNumber, TString)     -> TBool
    (OAbstractEq, TString, TNumber)     -> TBool
    (OAbstractEq, TBool, TNumber)       -> TBool
    (OAbstractEq, TNumber, TBool)       -> TBool
    (OAbstractEq, TNumber, TNumber)     -> TBool
    (OAbstractEq, TNull, TUndefined)    -> TBool
    (OAbstractEq, TUndefined, TNull)    -> TBool
    -- math operators
    (OMathPow, TNumber, TNumber)        -> TNumber
    -- object operators
    (OHasOwnProp, (TObject _), TString) -> TBool
    (_, TTop, _)                        -> TTop
    (_, _, TTop)                        -> TTop
    _                                   -> TTop) -< (op, v1, v2)
--  x -> fail -< "Unimplemented op: " ++ (show op) ++ ", params: " ++ (show v1) ++ ", " ++ (show v2)

typeEvalUnOp_ :: (ArrowChoice c) => c (Op, Type) Type
typeEvalUnOp_ = proc (op, vals) -> (arr $ \(op, vals) -> case (op, vals) of
    -- number operator
    (OToInteger, TNumber) -> TNumber
    (OToInt32, TNumber)   -> TNumber
    (OToUInt32, TNumber)  -> TNumber
    -- boolean operator
    (OBNot, TBool)        -> TBool
    -- string operators
    (OStrLen, TString)    -> TNumber
    -- isPrimitive operator
    (OIsPrim, _)          -> TBool
    -- primToNum operator
    -- #todo object conversions -> valueOf call
    (OPrimToNum, _)       -> TNumber
    -- primToStr operator
    (OPrimToStr, _)       -> TString
    -- primToBool operator
    (OPrimToBool, _)      -> TBool
    -- typeOf operator
    (OTypeof, _)          -> TString
    -- math operators
    (OMathExp, TNumber)   -> TNumber
    (OMathLog, TNumber)   -> TNumber
    (OMathCos, TNumber)   -> TNumber
    (OMathSin, TNumber)   -> TNumber
    (OMathAbs, TNumber)   -> TNumber
    _                     -> TTop) -< (op, vals)

fresh2 :: ArrowState Location c => c (Ident, Type', (M.Map Ident Location Type')) Location
fresh2 = proc _ -> do
    Location s <- Control.Arrow.State.get -< ()
    put -< Location $ s + 1
    returnA -< Location s

fresh :: ArrowState Location c => c () Location
fresh = proc () -> do
    Location s <- Control.Arrow.State.get -< ()
    put -< Location $ s + 1
    returnA -< Location s

getField_ :: ArrowChoice c => c (Type, String) Type'
getField_ = proc (t, s) -> do
    case t of
        TObject fs -> case find (\(n, _) -> n == s) fs of
            Just (_, fieldT) -> returnA -< fieldT
            Nothing          -> returnA -< P.singleton (TUndefined)
        _ -> returnA -< fromList [TObject [("0", P.singleton TString), ("length", P.singleton TNumber), ("$isArgs", P.singleton TBool)]]

instance {-# OVERLAPS #-} AbstractValue Type' (TypeArr s) where
    -- values
    numVal = proc _ -> returnA -< P.singleton TNumber
    boolVal = proc _ -> returnA -< P.singleton TBool
    stringVal = proc _ -> returnA -< P.singleton TString
    undefVal = proc () -> returnA -< P.singleton TUndefined
    nullVal = proc () -> returnA -< P.singleton TNull
    lambdaVal = proc (ids, body) -> do
        closure <- getEnv -< ()
        returnA -< P.singleton (TLambda ids body closure)
    objectVal = proc (fields) -> do
        returnA -< P.singleton (TObject fields)
    getField _ = proc (subject, field) -> do
        case field of
            EString name -> returnA -< Prelude.foldr P.union (P.empty) (Prelude.map getField_ (zip (toList subject) (repeat name)))
            _            -> returnA -< P.singleton TTop
    updateField _ = proc (_, _, _) -> returnA -< P.singleton TTop
    deleteField _ = proc (_, _) -> returnA -< P.singleton TTop
    -- operator/delta function
    evalOp = proc (op, vals) -> do
        case vals of
            [_] -> do
                t <- Control.Arrow.Utils.map typeEvalUnOp_ -< zip (repeat op) (toList $ head vals)
                returnA -< fromList t
            _ -> do
                let
                    v1 = toList (head vals)
                    v2 = toList (head $ tail vals)
                case length v1 == 1 && length v2 == 1 of
                    True -> do
                        t <- typeEvalBinOp_ -< (op, head v1, head v2)
                        returnA -< P.singleton t
                    False -> fail -< "Error: Binary op with set of type params not supported"
    -- environment ops
    lookup = proc id_ -> do
        Control.Arrow.Environment.lookup pi1 Control.Category.id -< (id_, P.singleton TUndefined)
    apply f1 = proc (lambdas, args) -> do
        ts <- Control.Arrow.Utils.map (proc (lambda, args) -> case lambda of
            TLambda names body closure
                | length names == length args -> do
                    --newBindings <- arr $ uncurry zip -< (names, args)
                    --closureEnv <- arr $ Prelude.foldr -< (M.insert, closure, M.empty)
                    --bindingEnv <- arr $ Prelude.foldr (\b m -> M.insert (first b, second b, m)) -< (closureEnv, newBindings)
                    --outsideEnv <- getEnv -< ()
                    --finalEnv <- bindings -< (bindingEnv, outsideEnv)
                    localEnv f1 -< (closure, body)
                | otherwise -> fail -< "Error: lambda must be applied with same amount of args as params"
            _ -> returnA -< fromList [TObject [("0", P.singleton TString), ("length", P.singleton TNumber), ("$isArgs", P.singleton TBool)]]) -< zip (toList lambdas) (repeat args)
        returnA -< Prelude.foldr P.union (P.empty) ts
    -- store ops
    set = proc (loc, val) -> do
        case toList loc of
            [TRef l] -> do
                Control.Arrow.Utils.map write -< zip (toList l) (repeat val)
                returnA -< ()
            _ -> fail -< "Error: EPRef lhs must be location"
    new = proc (val) -> do
        loc <- fresh >>> (arr (:[])) >>> (arr fromList) -< ()
        set -< (P.singleton (TRef loc), val)
        returnA -< P.singleton $ TRef loc
    get = proc (loc) -> do
        case toList loc of
            [TRef l] -> do
                vals <- Control.Arrow.Utils.map (read pi1 Control.Category.id) -< zip (toList l) (repeat (P.singleton TUndefined))
                returnA -< foldr1 (\x y -> x ⊔ y) vals
            _ -> returnA -< (P.singleton (TThrown (P.singleton (TObject []))))
    -- control flow
    if_ f1 f2 = proc (cond, thenBranch, elseBranch) -> do
        case toList cond of
            [TBool] -> do
                thenT <- f1 -< thenBranch
                elseT <- f2 -< elseBranch
                returnA -< P.union thenT elseT
            _ -> fail -< "Error: If conditional must be of type bool"
    while_ f1 f2 = proc (cond, body) -> do
        condT <- f1 -< cond
        case toList condT of
            [TBool] -> do
                f2 -< body
            _ -> fail -< "Error: While conditional must be of type bool"
    label f1 = proc (l, e) -> do
        eT <- f1 -< e
        case toList eT of
            [TBreak l1 t]
                | l == l1 -> returnA -< t
                | otherwise -> fail -< "Error: Expression within label must be of type break to that label"
            _ -> fail -< "Error: Expression within label must be of type break to that label"
    break = proc (l, t) -> do
        returnA -< P.singleton (TBreak l t)
    throw = proc t -> do
        returnA -< P.singleton (TThrown t)
    catch f1 = proc (try, _) -> do
        tryT <- f1 -< try
        case toList tryT of
            [TThrown t] -> returnA -< t
            _ -> fail -< "Error: Expression within try must be of type thrown"
    error = proc s -> fail -< "Error: aborted with message: " ++ s
