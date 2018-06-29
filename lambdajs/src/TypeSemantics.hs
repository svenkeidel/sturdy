{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module TypeSemantics where

import           Derivations
import           GHC.Generics                                    (Generic)
import           Prelude                                         hiding (break,
                                                                  error, fail,
                                                                  lookup, map,
                                                                  read)
import qualified Prelude
import           SharedInterpreter
import           Syntax

import           Data.Hashable
import           Data.Identifiable
import           Data.Set

import           Data.Abstract.Environment
import           Data.Abstract.HandleError
import           Data.Abstract.Store
import           Data.List                                       (find)
import           Data.Order

import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.HandleExcept
import           Control.Arrow.Transformer.Abstract.Store
import           Control.Arrow.Transformer.State
import           Control.Arrow.Utils                             (map, pi1, pi2)

import           Control.Arrow
import           Control.Arrow.Environment
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Store
import           Control.Category


instance PreOrd Type where
    TLambda ids1 _ e1 ⊑ TLambda ids2 _ e2 = ids1 == ids2 && e1 ⊑ e2
    TObject fields1 ⊑ TObject fields2 = all (\((f1, t1), (f2, t2)) -> f1 == f2 && t1 ⊑ t2) (zip fields1 fields2)
    TRef l1 ⊑ TRef l2 = Data.Set.isSubsetOf l1 l2
    TThrown t1 ⊑ TThrown t2 = Data.Set.isSubsetOf t1 t2
    TBreak l1 t1 ⊑ TBreak l2 t2 = Data.Set.isSubsetOf t1 t2 && l1 == l2
    a ⊑ b | a == b = True
    _ ⊑ _ = False



newtype TypeArr x y = TypeArr
    (Except
        String
        (Environment Ident Type'
            (StoreArrow Location Type'
                (State Location (->)))) x y)

deriving instance ArrowFail String TypeArr
deriving instance ArrowEnv Ident Type' (Env Ident Type') TypeArr
deriving instance Category TypeArr
deriving instance Arrow TypeArr
deriving instance ArrowChoice TypeArr
deriving instance ArrowRead Location Type' x Type' TypeArr
deriving instance ArrowWrite Location Type' TypeArr
deriving instance ArrowState Location TypeArr

runType :: TypeArr x y -> [(Ident, Type)] -> [(Location, Type)] -> x -> (Location, (Store Location Type', Error String y))
runType (TypeArr f) env env2 x = runState (runStore (runEnvironment (runExcept f))) (Location 0, (Data.Abstract.Store.fromList env2', (Data.Abstract.Environment.fromList env', x)))
        where env' = Prelude.map (\(x, y) -> (x, Data.Set.fromList [y])) env
              env2' = Prelude.map (\(x, y) -> (x, Data.Set.fromList [y])) env2

runAbstract :: [(Ident, Type)] -> [(Location, Type)] -> Expr -> (Store Location Type', Error String Type')
runAbstract env st exp = case runType eval env st exp of
    (l, (st, Fail e))      -> (st, Fail e)
    (l, (st, Success res)) -> (st, Success res)

typeEvalBinOp_ :: (ArrowFail String c, ArrowChoice c) => c (Op, Type, Type) Type
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
    (OStrictEq, a, b)                   -> TBool
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
    (_, _, TTop)                        -> TTop) -< (op, v1, v2)
--  x -> fail -< "Unimplemented op: " ++ (show op) ++ ", params: " ++ (show v1) ++ ", " ++ (show v2)

typeEvalUnOp_ :: (ArrowFail String c, ArrowChoice c) => c (Op, Type) Type
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
    (OMathAbs, TNumber)   -> TNumber) -< (op, vals)

fresh :: ArrowState Location c => c () Location
fresh = proc () -> do
    Location s <- Control.Arrow.State.get -< ()
    put -< Location $ s + 1
    returnA -< Location s

getField_ :: ArrowChoice c => c (Type, String) Type'
getField_ = proc (t, s) -> do
    case t of
        TObject fs -> case find (\(n, t) -> n == s) fs of
            Just (n, t) -> returnA -< t
            Nothing     -> returnA -< Data.Set.fromList [TUndefined]
        _ -> returnA -< Data.Set.fromList [TObject [("0", Data.Set.singleton TString), ("length", Data.Set.singleton TNumber), ("$isArgs", Data.Set.singleton TBool)]]



instance {-# OVERLAPS #-} AbstractValue Type' TypeArr where
    -- values
    numVal = proc _ -> returnA -< Data.Set.fromList [TNumber]
    boolVal = proc _ -> returnA -< Data.Set.fromList [TBool]
    stringVal = proc _ -> returnA -< Data.Set.fromList [TString]
    undefVal = proc () -> returnA -< Data.Set.fromList [TUndefined]
    nullVal = proc () -> returnA -< Data.Set.fromList [TNull]
    lambdaVal = proc (ids, body) -> do
        closure <- getEnv -< ()
        returnA -< Data.Set.fromList [TLambda ids body closure]
    objectVal = proc (fields) -> do
        returnA -< Data.Set.fromList [TObject fields]
    getField f1 = proc (subject, field) -> do
        case field of
            EString name -> returnA -< Prelude.foldr Data.Set.union (Data.Set.empty) (Prelude.map getField_ (zip (Data.Set.toList subject) (repeat name)))
            _            -> returnA -< singleton TTop
    updateField f1 = proc (_, _, _) -> returnA -< singleton TTop
    deleteField f1 = proc (_, _) -> returnA -< singleton TTop
    -- operator/delta function
    evalOp = proc (op, vals) -> do
        case vals of
            [_] -> do
                t <- Control.Arrow.Utils.map typeEvalUnOp_ -< zip (repeat op) (Data.Set.toList $ head vals)
                returnA -< Data.Set.fromList t
            _ -> do
                let
                    v1 = Data.Set.toList (head vals)
                    v2 = Data.Set.toList (head $ tail vals)
                case length v1 == 1 && length v2 == 1 of
                    True -> do
                        t <- typeEvalBinOp_ -< (op, head v1, head v2)
                        returnA -< Data.Set.fromList [t]
                    False -> fail -< "Error: Binary op with set of type params not supported"
    -- environment ops
    lookup = proc id -> do
        Control.Arrow.Environment.lookup pi1 Control.Category.id -< (id, Data.Set.singleton TUndefined)
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
            _ -> returnA -< Data.Set.fromList [TObject [("0", singleton TString), ("length", singleton TNumber), ("$isArgs", singleton TBool)]]) -< zip (Data.Set.toList lambdas) (repeat args)
        returnA -< Prelude.foldr Data.Set.union (Data.Set.empty) ts
    -- store ops
    set = proc (loc, val) -> do
        case Data.Set.toList loc of
            [TRef l] -> do
                Control.Arrow.Utils.map write -< zip (Data.Set.toList l) (repeat val)
                returnA -< ()
            _ -> fail -< "Error: ESetRef lhs must be location"
    new = proc (val) -> do
        loc <- fresh >>> (arr (:[])) >>> (arr Data.Set.fromList) -< ()
        set -< (Data.Set.fromList [TRef loc], val)
        returnA -< Data.Set.fromList [TRef loc]
    get = proc (loc) -> do
        case Data.Set.toList loc of
            [TRef l] -> do
                vals <- Control.Arrow.Utils.map (read pi1 Control.Category.id) -< zip (Data.Set.toList l) (repeat (Data.Set.singleton TUndefined))
                returnA -< foldr1 (\x y -> x ⊔ y) vals
            _ -> returnA -< (Data.Set.fromList [TThrown (Data.Set.fromList [TObject []])])
    -- control flow
    if_ f1 f2 = proc (cond, thenBranch, elseBranch) -> do
        case Data.Set.toList cond of
            [TBool] -> do
                thenT <- f1 -< thenBranch
                elseT <- f2 -< elseBranch
                returnA -< Data.Set.union thenT elseT
            _ -> fail -< "Error: If conditional must be of type bool"
    while_ f1 f2 = proc (cond, body) -> do
        condT <- f1 -< cond
        case Data.Set.toList condT of
            [TBool] -> do
                f2 -< body
            _ -> fail -< "Error: While conditional must be of type bool"
    label f1 = proc (l, e) -> do
        eT <- f1 -< e
        case Data.Set.toList eT of
            [TBreak l1 t]
                | l == l1 -> returnA -< t
                | otherwise -> fail -< "Error: Expression within label must be of type break to that label"
            _ -> fail -< "Error: Expression within label must be of type break to that label"
    break = proc (l, t) -> do
        returnA -< singleton (TBreak l t)
    throw = proc t -> do
        returnA -< singleton (TThrown t)
    catch f1 = proc (try, catch) -> do
        tryT <- f1 -< try
        case Data.Set.toList tryT of
            [TThrown t] -> returnA -< t
            _ -> fail -< "Error: Expression within try must be of type thrown"
    error = proc s -> fail -< "Error: aborted with message: " ++ s
