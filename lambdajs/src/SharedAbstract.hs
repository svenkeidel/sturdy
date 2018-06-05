{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverlappingInstances #-}
module SharedAbstract where

import Prelude hiding(lookup, break, read, error)
import qualified Prelude
import GHC.Generics (Generic)
import Syntax
import SharedInterpreter
import Derivations

import Data.Hashable
import Data.Set
import Data.Identifiable

import Data.Abstract.Environment
import Data.Abstract.Store
import Data.Abstract.HandleError
import Data.Order
import Data.HashMap.Lazy

import Control.Arrow.Transformer.Abstract.HandleExcept
import Control.Arrow.Transformer.Abstract.Environment
import Control.Arrow.Transformer.Abstract.Store
import Control.Arrow.Transformer.State
import Control.Arrow.Utils (mapA, pi2, pi1)

import Control.Arrow.Store
import Control.Arrow.Environment
import Control.Arrow.Fail
import Control.Arrow.State
import Control.Arrow.Reader
import Control.Arrow.Except
import Control.Arrow
import Control.Category

instance PreOrd Type where
    TLambda ids1 t1 ⊑ TLambda ids2 t2 = ids1 == ids2 && t1 ⊑ t2 
    TObject fields1 ⊑ TObject fields2 = all (\((f1, t1), (f2, t2)) -> f1 == f2 && t1 ⊑ t2) (zip fields1 fields2)
    TRef l1 ⊑ TRef l2 = Data.Set.isSubsetOf l1 l2
    TThrown t1 ⊑ TThrown t2 = Data.Set.isSubsetOf t1 t2  
    TBreak l1 t1 ⊑ TBreak l2 t2 = Data.Set.isSubsetOf t1 t2 && l1 == l2
    a ⊑ b | a == b = True
    _ ⊑ _ = False



newtype TypeArr x y = TypeArr (Except String (Environment Ident Location' (StoreArrow Location Type' (State Location (->)))) x y)
deriving instance ArrowFail String TypeArr
deriving instance ArrowEnv Ident Location' (Env Ident Location') TypeArr
deriving instance Category TypeArr
deriving instance Arrow TypeArr
deriving instance ArrowChoice TypeArr

instance (Show Location, Identifiable Type, ArrowChoice c) => ArrowStore Location Type' lab (StoreArrow Location Type' c) where
  read =
    StoreArrow $ State $ proc (s,(var,_)) -> case Data.Abstract.Store.lookup var s of
      Just v -> returnA -< (s,v)
      Nothing -> returnA -< (s, Data.Set.fromList [TUndefined])
  write = StoreArrow (State (arr (\(s,(x,v,_)) -> (Data.Abstract.Store.insert x v s,()))))
deriving instance ArrowStore Location Type' () TypeArr
instance (Show Ident, Identifiable Ident, ArrowChoice c) => ArrowEnv Ident Location' (Env Ident Location') (Environment Ident Location' c) where
  lookup = proc x -> do
    env <- getEnv -< ()
    case Data.Abstract.Environment.lookup x env of
      Just y -> returnA -< y
      Nothing -> returnA -< Data.Set.fromList $ [Location 0]
  getEnv = Environment askA
  extendEnv = arr $ \(x,y,env) -> Data.Abstract.Environment.insert x y env
  localEnv (Environment f) = Environment (localA f)
deriving instance ArrowState Location TypeArr

runType :: TypeArr x y -> [(Ident, Location)] -> [(Location, Type)] -> x -> (Location, (Store Location Type', Error String y))
runType (TypeArr f) env env2 x = runState (runStore (runEnvironment (runExcept f))) (Location 0, (Data.Abstract.Store.fromList env2', (env', x)))
        where env' = Prelude.map (\(x, y) -> (x, Data.Set.fromList [y])) env
              env2' = Prelude.map (\(x, y) -> (x, Data.Set.fromList [y])) env2

runAbstract :: [(Ident, Location)] -> [(Location, Type)] -> Expr -> (Store Location Type', Error String Type')
runAbstract env st exp = case runType eval env st exp of
    (l, (st, Fail e)) -> (st, Fail e)
    (l, (st, Success res)) -> (st, Success res)

typeEvalBinOp_ :: (ArrowFail String c, ArrowChoice c) => c (Op, Type, Type) Type
typeEvalBinOp_ = proc (op, v1, v2) -> case (op, v1, v2) of 
    -- number operators
    (ONumPlus, TNumber, TNumber) -> returnA -< TNumber
    (OMul, TNumber, TNumber) -> returnA -< TNumber
    (ODiv, TNumber, TNumber) -> returnA -< TNumber
    (OMod, TNumber, TNumber) -> returnA -< TNumber
    (OSub, TNumber, TNumber) -> returnA -< TNumber
    (OLt, TNumber, TNumber) -> returnA -< TBool
    -- shift operators
    (OLShift, TNumber, TNumber) -> returnA -< TNumber
    (OSpRShift, TNumber, TNumber) -> returnA -< TNumber
    (OZfRShift, TNumber, TNumber) -> returnA -< TNumber
    -- string operators
    (OStrPlus, TString, TString) -> returnA -< TString
    (OStrLt, TString, TString) -> returnA -< TBool
    -- boolean operators
    (OBAnd, TBool, TBool) -> returnA -< TBool
    (OBOr, TBool, TBool) -> returnA -< TBool
    (OBXOr, TBool, TBool) -> returnA -< TBool
    -- equality operators
    (OStrictEq, a, b) -> returnA -< TBool
    (OAbstractEq, TNumber, TString) -> returnA -< TBool
    (OAbstractEq, TString, TNumber) -> returnA -< TBool
    (OAbstractEq, TBool, TNumber) -> returnA -< TBool
    (OAbstractEq, TNumber, TBool) -> returnA -< TBool
    (OAbstractEq, TNumber, TNumber) -> returnA -< TBool
    (OAbstractEq, TNull, TUndefined) -> returnA -< TBool
    (OAbstractEq, TUndefined, TNull) -> returnA -< TBool
    -- math operators
    (OMathPow, TNumber, TNumber) -> returnA -< TNumber
    -- object operators
    (OHasOwnProp, (TObject _), TString) -> returnA -< TBool

typeEvalUnOp_ :: (ArrowFail String c, ArrowChoice c) => c (Op, Type) Type
typeEvalUnOp_ = proc (op, vals) -> case (op, vals) of
    -- number operator
    (OToInteger, TNumber) -> returnA -< TNumber
    (OToInt32, TNumber) -> returnA -< TNumber
    (OToUInt32, TNumber) -> returnA -< TNumber
    -- boolean operator
    (OBNot, TBool) -> returnA -< TBool
    -- string operators
    (OStrLen, TString) -> returnA -< TNumber
    -- isPrimitive operator
    (OIsPrim, _) -> returnA -< TBool
    -- primToNum operator
    -- #todo object conversions -> valueOf call
    (OPrimToNum, _) -> returnA -< TNumber
    -- primToStr operator
    (OPrimToStr, _) -> returnA -< TString
    -- primToBool operator
    (OPrimToBool, _) -> returnA -< TBool
    -- typeOf operator
    (OTypeof, _) -> returnA -< TString 
    -- math operators
    (OMathExp, TNumber) -> returnA -< TNumber
    (OMathLog, TNumber) -> returnA -< TNumber
    (OMathCos, TNumber) -> returnA -< TNumber
    (OMathSin, TNumber) -> returnA -< TNumber
    (OMathAbs, TNumber) -> returnA -< TNumber

fresh :: ArrowState Location c => c () Location 
fresh = proc () -> do
    Location s <- getA -< ()
    putA -< Location $ s + 1
    returnA -< Location $ s + 1

instance {-# OVERLAPS #-} AbstractValue Type' TypeArr where
    -- values
    numVal = proc _ -> returnA -< Data.Set.fromList [TNumber]
    boolVal = proc _ -> returnA -< Data.Set.fromList [TBool]
    stringVal = proc _ -> returnA -< Data.Set.fromList [TString]
    undefVal = proc () -> returnA -< Data.Set.fromList [TUndefined]
    nullVal = proc () -> returnA -< Data.Set.fromList [TNull]
    lambdaVal = proc (ids, body) -> do
        bodyT <- eval -< body
        returnA -< Data.Set.fromList [TLambda ids bodyT]
    objectVal = proc (fields) -> do
        returnA -< Data.Set.fromList [TObject fields]
    getField = proc (_, _) -> returnA -< Data.Set.fromList [TTop]
    updateField = proc (_, _, _) -> returnA -< Data.Set.fromList [TTop]
    deleteField = proc (_, _) -> returnA -< Data.Set.fromList [TTop]
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
        loc <- Control.Arrow.Environment.lookup -< id
        returnA -< Data.Set.fromList [TRef loc]
    apply f1 = proc (lambdas, args) -> do
        ts <- mapA (proc (lambda, args) -> case lambda of
            TLambda names bodyT -> do
                case length names == length args of
                    False -> failA -< "Error: lambda must be applied with same amount of args as params"
                    True -> returnA -< bodyT
            _ -> failA -< "Error: apply must take lambda") -< zip (Data.Set.toList lambdas) (repeat args)
        returnA -< foldr1 (\x y -> x ⊔ y) ts
        -- generate locations for arguments equal to length of pairs 
        --locations <- mapA ((arr $ const ()) >>> fresh) -< args

        --forStore <- arr $ uncurry zip -< (locations, args) 
        --mapA set -< Prelude.map (\(a, b) -> (TRef a, b)) forStore

        --forEnv <- arr $ uncurry zip -< (names, locations) 
        --scope <- getEnv -< ()
        --env' <- bindings -< (forEnv, scope)
        --localEnv eval -< (env', bodyT)
        -- Just return bodyT for now, in the future typecheck the body with parameter types known
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
            _ -> failA -< "Error: EDeref lhs must be location"
    -- control flow
    if_ f1 f2 = proc (cond, thenBranch, elseBranch) -> do
        case Data.Set.toList cond of
            [TBool] -> do
                thenT <- f1 -< thenBranch
                elseT <- f2 -< elseBranch
                returnA -< Data.Set.union thenT elseT
            _ -> failA -< "Error: Conditional must be of type bool"
    label f1 = proc (l, e) -> do
        eT <- f1 -< e
        case Data.Set.toList eT of
            [TBreak l1 t] -> case l == l1 of
                True -> returnA -< t
                False -> failA -< "Error: Expression within label must be of type break to that label"
            _ -> failA -< "Error: Expression within label must be of type break to that label"
    break = proc (l, t) -> do
        returnA -< Data.Set.fromList $ [TBreak l t]
    throw = proc t -> do
        returnA -< Data.Set.fromList $ [TThrown t]
    catch f1 = proc (try, catch) -> do
        tryT <- f1 -< try
        case Data.Set.toList tryT of
            [TThrown t] -> returnA -< t
            _ -> failA -< "Error: Expression within try must be of type thrown"
    error = proc s -> failA -< "Error: aborted with message: " ++ s
