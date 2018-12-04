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

import Data.Hashable
import Data.Identifiable

import Data.Abstract.Environment
import Data.Abstract.Store
import Data.Abstract.HandleError
import Data.Order

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

-- Objects might join 
data Type
    = TNumber
    | TString
    | TBool
    | TUndefined
    | TNull
    | TLambda [Ident] Type
    | TObject [(Ident, Type)]
    | TTop
    | TBottom
    | TRef Location
    | TThrown Type
    | TBreak Label Type
    deriving (Show, Eq, Generic)
instance Hashable Type

newtype TypeArr x y = TypeArr (Except String (Environment Ident Location (StoreArrow Location Type (State Location (->)))) x y)
deriving instance ArrowFail String TypeArr
deriving instance ArrowEnv Ident Location (Env Ident Location) TypeArr
deriving instance Category TypeArr
deriving instance Arrow TypeArr
deriving instance ArrowChoice TypeArr

instance (Show Location, Identifiable Type, ArrowChoice c) => ArrowStore Location Type lab (StoreArrow Location Type c) where
  read =
    StoreArrow $ State $ proc (s,(var,_)) -> case Data.Abstract.Store.lookup var s of
      Just v -> returnA -< (s,v)
      Nothing -> returnA -< (s, TUndefined)
  write = StoreArrow (State (arr (\(s,(x,v,_)) -> (Data.Abstract.Store.insert x v s,()))))
instance (Show Ident, Identifiable Ident, ArrowChoice c) => ArrowEnv Ident Location (Env Ident Location) (Environment Ident Location c) where
  lookup = proc x -> do
    env <- getEnv -< ()
    case Data.Abstract.Environment.lookup x env of
      Just y -> returnA -< y
      Nothing -> returnA -< Location 0
  getEnv = Environment askA
  extendEnv = arr $ \(x,y,env) -> Data.Abstract.Environment.insert x y env
  localEnv (Environment f) = Environment (localA f)
deriving instance ArrowStore Location Type () TypeArr
deriving instance ArrowState Location TypeArr


runType :: TypeArr x y -> [(Ident, Location)] -> [(Location, Type)] -> x -> (Location, (Store Location Type, Error String y))
runType (TypeArr f) env env2 x = runState (runStore (runEnvironment (runExcept f))) (Location 0, (Data.Abstract.Store.fromList env2, (env, x)))

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

fresh :: ArrowState Location c => c () Location 
fresh = proc () -> do
    Location s <- getA -< ()
    putA -< Location $ s + 1
    returnA -< Location $ s + 1

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
    apply = proc (TLambda names bodyT, args) -> do
        case length names == length args of
            False ->  failA -< "Error: applied lambda with less/more params than arguments"
            True -> do
                -- generate locations for arguments equal to length of pairs 
                locations <- mapA ((arr $ const ()) >>> fresh) -< args

                forStore <- arr $ uncurry zip -< (locations, args) 
                mapA set -< Prelude.map (\(a, b) -> (TRef a, b)) forStore

                forEnv <- arr $ uncurry zip -< (names, locations) 
                scope <- getEnv -< ()
                env' <- bindings -< (forEnv, scope)
                --localEnv eval -< (env', bodyT)
                -- Just return bodyT for now, in the future typecheck the body with parameter types known
                returnA -< bodyT
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
    error = proc s -> failA -< "Error: aborted with message: " ++ s
