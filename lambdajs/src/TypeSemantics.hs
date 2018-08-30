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

import           Data.Abstract.Bounded
import           Data.Abstract.Environment
import           Data.Abstract.HandleError
import qualified Data.Abstract.Powerset                                as P
import           Data.Abstract.Store
import           Data.Abstract.Terminating
import           Data.List                                             (find)
import           Data.Order
import           Data.Set

import           Control.Arrow.Transformer.Abstract.BoundedEnvironment
import           Control.Arrow.Transformer.Abstract.Contour
import           Control.Arrow.Transformer.Abstract.HandleExcept
import           Control.Arrow.Transformer.Abstract.LeastFixPoint
import           Control.Arrow.Transformer.State
import           Control.Arrow.Utils                                   (map,
                                                                        pi1)

import           Control.Arrow
import           Control.Arrow.Environment
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.State
import           Control.Arrow.Store
import           Control.Category

newtype TypeArr x y = TypeArr
    (Fix Expr Type'
        (Except String
            (Environment Ident Location Type'
                (Contour
                    (State Location
                        (LeastFix () ()
                            (->)))))) x y)

deriving instance ArrowFail String TypeArr
deriving instance ArrowEnv Ident Type' (Env Ident Type') TypeArr
deriving instance Category TypeArr
deriving instance Arrow TypeArr
deriving instance ArrowChoice TypeArr
deriving instance ArrowRead Location Type' x Type' TypeArr
deriving instance ArrowWrite Location Type' TypeArr
deriving instance ArrowState Location TypeArr
deriving instance ArrowFix Expr Type' TypeArr

runType :: TypeArr x y -> [(Ident, Type)] -> x -> Terminating (Location, Error String y)
runType (TypeArr f) env x =
    runLeastFix
        (runState
            (runContour 0
                (runEnvironment
                    (runExcept f))))
    (Location 0, (env', x))
    where env' = Prelude.map (\(a, b) -> (a, P.singleton b)) env

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
            Nothing          -> returnA -< P.fromFoldable [TUndefined]
        _ -> returnA -< P.fromFoldable [TObject [("0", P.singleton TString), ("length", P.singleton TNumber), ("$isArgs", P.singleton TBool)]]



instance {-# OVERLAPS #-} AbstractValue Type' TypeArr where
    -- values
    numVal = proc _ -> returnA -< P.singleton TNumber
    boolVal = proc _ -> returnA -< P.fromFoldable [TBool]
    stringVal = proc _ -> returnA -< P.fromFoldable [TString]
    undefVal = proc () -> returnA -< P.fromFoldable [TUndefined]
    nullVal = proc () -> returnA -< P.fromFoldable [TNull]
    lambdaVal = proc (ids, body) -> do
        closure <- getEnv -< ()
        returnA -< P.fromFoldable [TLambda ids body closure]
    objectVal = proc (fields) -> do
        returnA -< P.fromFoldable [TObject fields]
    getField _ = proc (subject, field) -> do
        case field of
            EString name -> returnA -< Prelude.foldr P.union (P.empty) (Prelude.map getField_ (zip (P.toList subject) (repeat name)))
            _            -> returnA -< P.singleton TTop
    updateField _ = proc (_, _, _) -> returnA -< P.singleton TTop
    deleteField _ = proc (_, _) -> returnA -< P.singleton TTop
    -- operator/delta function
    evalOp = proc (op, vals) -> do
        case vals of
            [_] -> do
                t <- Control.Arrow.Utils.map typeEvalUnOp_ -< zip (repeat op) (P.toList $ head vals)
                returnA -< P.fromFoldable t
            _ -> do
                let
                    v1 = P.toList (head vals)
                    v2 = P.toList (head $ tail vals)
                case length v1 == 1 && length v2 == 1 of
                    True -> do
                        t <- typeEvalBinOp_ -< (op, head v1, head v2)
                        returnA -< P.fromFoldable [t]
                    False -> fail -< "Error: Binary op with set of type params not supported"
    -- environment ops
    lookup = proc id_ -> do
        Control.Arrow.Environment.lookup pi1 Control.Category.id -< (id_, P.singleton TUndefined)
    apply f1 = proc (lambdas, args) -> do
        ts <- Control.Arrow.Utils.map (proc (lambda, args) -> case lambda of
            TLambda names body closureEnv
                | length names == length args -> do
                    newBindings <- arr $ uncurry zip -< (names, args)
                    bindingEnv <- bindings -< (newBindings, closureEnv)
                    outsideEnv <- getEnv -< ()
                    finalEnv <- bindings -< (Data.Abstract.Environment.toList bindingEnv, outsideEnv)
                    localEnv f1 -< (finalEnv, body)
                | otherwise -> fail -< "Error: lambda must be applied with same amount of args as params"
            _ -> returnA -< P.fromFoldable [TObject [("0", P.singleton TString), ("length", P.singleton TNumber), ("$isArgs", P.singleton TBool)]]) -< zip (P.toList lambdas) (repeat args)
        returnA -< Prelude.foldr P.union (P.empty) ts
    -- store ops
    set = proc (loc, val) -> do
        case P.toList loc of
            [TRef l] -> do
                Control.Arrow.Utils.map write -< zip (Data.Set.toList l) (repeat val)
                returnA -< ()
            _ -> fail -< "Error: ESetRef lhs must be location"
    new = proc (val) -> do
        loc <- fresh >>> (arr (:[])) >>> (arr Data.Set.fromList) -< ()
        set -< (P.fromFoldable [TRef loc], val)
        returnA -< P.singleton $ TRef loc
    get = proc (loc) -> do
        case P.toList loc of
            [TRef l] -> do
                vals <- Control.Arrow.Utils.map (read pi1 Control.Category.id) -< zip (Data.Set.toList l) (repeat (P.singleton TUndefined))
                returnA -< foldr1 (\x y -> x ⊔ y) vals
            _ -> returnA -< (P.fromFoldable [TThrown (P.fromFoldable [TObject []])])
    -- control flow
    if_ f1 f2 = proc (cond, thenBranch, elseBranch) -> do
        case P.toList cond of
            [TBool] -> do
                thenT <- f1 -< thenBranch
                elseT <- f2 -< elseBranch
                returnA -< P.union thenT elseT
            _ -> fail -< "Error: If conditional must be of type bool"
    while_ f1 f2 = proc (cond, body) -> do
        condT <- f1 -< cond
        case P.toList condT of
            [TBool] -> do
                f2 -< body
            _ -> fail -< "Error: While conditional must be of type bool"
    label f1 = proc (l, e) -> do
        eT <- f1 -< e
        case P.toList eT of
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
        case P.toList tryT of
            [TThrown t] -> returnA -< t
            _ -> fail -< "Error: Expression within try must be of type thrown"
    error = proc s -> fail -< "Error: aborted with message: " ++ s
