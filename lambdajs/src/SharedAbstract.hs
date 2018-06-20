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
module SharedAbstract where

import           Derivations
import           GHC.Generics                                    (Generic)
import           Prelude                                         hiding (break,
                                                                  error, lookup,
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
import           Control.Arrow.Utils                             (mapA, pi1,
                                                                  pi2)

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



newtype TypeArr x y = TypeArr (Except String (Environment Ident Type' (StoreArrow Location Type' (State Location (->)))) x y)
deriving instance ArrowFail String TypeArr
deriving instance ArrowEnv Ident Type' (Env Ident Type') TypeArr
deriving instance Category TypeArr
deriving instance Arrow TypeArr
deriving instance ArrowChoice TypeArr

instance (Show Location, Identifiable Type, ArrowChoice c) => ArrowStore Location Type' lab (StoreArrow Location Type' c) where
  read =
    StoreArrow $ State $ proc (s,(var,_)) -> case Data.Abstract.Store.lookup var s of
      Just v  -> returnA -< (s,v)
      Nothing -> returnA -< (s, Data.Set.fromList [TUndefined])
  write = StoreArrow (State (arr (\(s,(x,v,_)) -> (Data.Abstract.Store.insert x v s,()))))
deriving instance ArrowStore Location Type' () TypeArr
instance (Show Ident, Identifiable Ident, ArrowChoice c) => ArrowEnv Ident Type' (Env Ident Type') (Environment Ident Type' c) where
  lookup = proc x -> do
    env <- getEnv -< ()
    case Data.Abstract.Environment.lookup x env of
      Just y  -> returnA -< y
      Nothing -> returnA -< Data.Set.fromList $ [TRef $ Data.Set.fromList [Location 0]]
  getEnv = Environment askA
  extendEnv = arr $ \(x,y,env) -> Data.Abstract.Environment.insert x y env
  localEnv (Environment f) = Environment (localA f)
deriving instance ArrowState Location TypeArr

runType :: TypeArr x y -> [(Ident, Type)] -> [(Location, Type)] -> x -> (Location, (Store Location Type', Error String y))
runType (TypeArr f) env env2 x = runState (runStore (runEnvironment (runExcept f))) (Location 0, (Data.Abstract.Store.fromList env2', (env', x)))
        where env' = Prelude.map (\(x, y) -> (x, Data.Set.fromList [y])) env
              env2' = Prelude.map (\(x, y) -> (x, Data.Set.fromList [y])) env2

runAbstract :: [(Ident, Type)] -> [(Location, Type)] -> Expr -> (Store Location Type', Error String Type')
runAbstract env st exp = case runType eval env st exp of
    (l, (st, Fail e))      -> (st, Fail e)
    (l, (st, Success res)) -> (st, Success res)

typeEvalBinOp_ :: (ArrowFail String c, ArrowChoice c) => c (Op, Type, Type) Type
typeEvalBinOp_ = proc (op, v1, v2) -> case (op, v1, v2) of
    -- number operators
    (ONumPlus, TNumber, TNumber)        -> returnA -< TNumber
    (OMul, TNumber, TNumber)            -> returnA -< TNumber
    (ODiv, TNumber, TNumber)            -> returnA -< TNumber
    (OMod, TNumber, TNumber)            -> returnA -< TNumber
    (OSub, TNumber, TNumber)            -> returnA -< TNumber
    (OLt, TNumber, TNumber)             -> returnA -< TBool
    -- shift operators
    (OLShift, TNumber, TNumber)         -> returnA -< TNumber
    (OSpRShift, TNumber, TNumber)       -> returnA -< TNumber
    (OZfRShift, TNumber, TNumber)       -> returnA -< TNumber
    -- string operators
    (OStrPlus, TString, TString)        -> returnA -< TString
    (OStrLt, TString, TString)          -> returnA -< TBool
    -- boolean operators
    (OBAnd, TBool, TBool)               -> returnA -< TBool
    (OBOr, TBool, TBool)                -> returnA -< TBool
    (OBXOr, TBool, TBool)               -> returnA -< TBool
    -- equality operators
    (OStrictEq, a, b)                   -> returnA -< TBool
    (OAbstractEq, TNumber, TString)     -> returnA -< TBool
    (OAbstractEq, TString, TNumber)     -> returnA -< TBool
    (OAbstractEq, TBool, TNumber)       -> returnA -< TBool
    (OAbstractEq, TNumber, TBool)       -> returnA -< TBool
    (OAbstractEq, TNumber, TNumber)     -> returnA -< TBool
    (OAbstractEq, TNull, TUndefined)    -> returnA -< TBool
    (OAbstractEq, TUndefined, TNull)    -> returnA -< TBool
    -- math operators
    (OMathPow, TNumber, TNumber)        -> returnA -< TNumber
    -- object operators
    (OHasOwnProp, (TObject _), TString) -> returnA -< TBool
    (_, TTop, _) -> returnA -< TTop
    (_, _, TTop) -> returnA -< TTop
    x -> failA -< "Unimplemented op: " ++ (show op) ++ ", params: " ++ (show v1) ++ ", " ++ (show v2)

typeEvalUnOp_ :: (ArrowFail String c, ArrowChoice c) => c (Op, Type) Type
typeEvalUnOp_ = proc (op, vals) -> case (op, vals) of
    -- number operator
    (OToInteger, TNumber) -> returnA -< TNumber
    (OToInt32, TNumber)   -> returnA -< TNumber
    (OToUInt32, TNumber)  -> returnA -< TNumber
    -- boolean operator
    (OBNot, TBool)        -> returnA -< TBool
    -- string operators
    (OStrLen, TString)    -> returnA -< TNumber
    -- isPrimitive operator
    (OIsPrim, _)          -> returnA -< TBool
    -- primToNum operator
    -- #todo object conversions -> valueOf call
    (OPrimToNum, _)       -> returnA -< TNumber
    -- primToStr operator
    (OPrimToStr, _)       -> returnA -< TString
    -- primToBool operator
    (OPrimToBool, _)      -> returnA -< TBool
    -- typeOf operator
    (OTypeof, _)          -> returnA -< TString
    -- math operators
    (OMathExp, TNumber)   -> returnA -< TNumber
    (OMathLog, TNumber)   -> returnA -< TNumber
    (OMathCos, TNumber)   -> returnA -< TNumber
    (OMathSin, TNumber)   -> returnA -< TNumber
    (OMathAbs, TNumber)   -> returnA -< TNumber

fresh :: ArrowState Location c => c () Location
fresh = proc () -> do
    Location s <- getA -< ()
    putA -< Location $ s + 1
    returnA -< Location $ s + 1

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
            EString name -> returnA -< Prelude.foldr Data.Set.union (Data.Set.empty) (mapA getField_ (zip (Data.Set.toList subject) (repeat name)))
            _            -> returnA -< singleton TTop
    updateField f1 = proc (_, _, _) -> returnA -< singleton TTop
    deleteField f1 = proc (_, _) -> returnA -< singleton TTop
    -- operator/delta function
    evalOp = proc (op, vals) -> do
        case length vals of
            1 -> do
                t <- mapA typeEvalUnOp_ -< zip (repeat op) (Data.Set.toList $ head vals)
                returnA -< Data.Set.fromList t
            2 -> do
                let
                    v1 = Data.Set.toList (head vals)
                    v2 = Data.Set.toList (head $ tail vals)
                case length v1 == 1 && length v2 == 1 of
                    True -> do
                        t <- typeEvalBinOp_ -< (op, head v1, head v2)
                        returnA -< Data.Set.fromList [t]
                    False -> failA -< "Error: Binary op with set of type params not supported"
    -- environment ops
    lookup = proc id -> do
        Control.Arrow.Environment.lookup -< id
    apply f1 = proc (lambdas, args) -> do
        ts <- mapA (proc (lambda, args) -> case lambda of
            TLambda names body closureEnv -> do
                case length names == length args of
                    False -> failA -< "Error: lambda must be applied with same amount of args as params"
                    True -> do
                        newBindings <- arr $ uncurry zip -< (names, args)
                        bindingEnv <- bindings -< (newBindings, closureEnv)
                        outsideEnv <- getEnv -< ()
                        finalEnv <- bindings -< (Data.Abstract.Environment.toList bindingEnv, outsideEnv)
                        localEnv f1 -< (finalEnv, body)
            _ -> returnA -< Data.Set.fromList [TObject [("0", singleton TString), ("length", singleton TNumber), ("$isArgs", singleton TBool)]]) -< zip (Data.Set.toList lambdas) (repeat args)
        returnA -< Prelude.foldr Data.Set.union (Data.Set.empty) ts
    -- store ops
    set = proc (loc, val) -> do
        case Data.Set.toList loc of
            [TRef l] -> do
                mapA write -< zip3 (Data.Set.toList l) (repeat val) (repeat ())
                returnA -< ()
            _ -> failA -< "Error: ESetRef lhs must be location"
    new = proc (val) -> do
        loc <- fresh >>> (arr (:[])) >>> (arr Data.Set.fromList) -< ()
        set -< (Data.Set.fromList [TRef loc], val)
        returnA -< Data.Set.fromList [TRef loc]
    get = proc (loc) -> do
        case Data.Set.toList loc of
            [TRef l] -> do
                vals <- mapA read -< zip (Data.Set.toList l) (repeat ())
                returnA -< foldr1 (\x y -> x ⊔ y) vals
            _ -> returnA -< (Data.Set.fromList [TThrown (Data.Set.fromList [TObject []])])
    -- control flow
    if_ f1 f2 = proc (cond, thenBranch, elseBranch) -> do
        case Data.Set.toList cond of
            [TBool] -> do
                thenT <- f1 -< thenBranch
                elseT <- f2 -< elseBranch
                returnA -< Data.Set.union thenT elseT
            _ -> failA -< "Error: If conditional must be of type bool"
    while_ f1 f2 = proc (cond, body) -> do
        condT <- f1 -< cond
        case Data.Set.toList condT of
            [TBool] -> do
                f2 -< body
            _ -> failA -< "Error: While conditional must be of type bool"
    label f1 = proc (l, e) -> do
        eT <- f1 -< e
        case Data.Set.toList eT of
            [TBreak l1 t] -> case l == l1 of
                True -> returnA -< t
                False -> failA -< "Error: Expression within label must be of type break to that label"
            _ -> failA -< "Error: Expression within label must be of type break to that label"
    break = proc (l, t) -> do
        returnA -< singleton (TBreak l t)
    throw = proc t -> do
        returnA -< singleton (TThrown t)
    catch f1 = proc (try, catch) -> do
        tryT <- f1 -< try
        case Data.Set.toList tryT of
            [TThrown t] -> returnA -< t
            _ -> failA -< "Error: Expression within try must be of type thrown"
    error = proc s -> failA -< "Error: aborted with message: " ++ s
