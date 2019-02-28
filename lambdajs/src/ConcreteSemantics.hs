{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module ConcreteSemantics where

import           GHC.Generics (Generic)
import           Prelude hiding (break, error, fail, id, (.), lookup, map, read)
import qualified Prelude

import           SharedInterpreter hiding (eval)
import qualified SharedInterpreter as Shared
import           Syntax

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Alloc
import           Control.Arrow.Conditional
import           Control.Arrow.Fail
import           Control.Arrow.Environment
import           Control.Arrow.Except
import           Control.Arrow.Fix
import           Control.Arrow.Store as Store
import           Control.Arrow.Transformer.Concrete.Environment
import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Concrete.Failure
import           Control.Arrow.Transformer.Concrete.Store

import           Data.Bits (shift)
import           Data.Fixed (mod')
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Label as Lab
import           Data.List
import           Data.List.Split (splitOn)
import           Data.Profunctor
import           Data.String
import           Data.Word (Word32)

import           Data.Concrete.Error

import           Text.Printf
import           Text.Read (readMaybe)

type Env = HashMap Ident Value

data Value
    = VNumber Double
    | VString String
    | VBool Bool
    | VUndefined
    | VNull
    | VLambda [Ident] Expr Env
    | VObject [String] (HashMap String Value)
    | VRef Addr
    deriving (Show, Eq, Generic)

objectFromList :: [(String, Value)] -> Value
objectFromList keyVals = VObject (filter (not . isPrefixOf "$") $ map fst keyVals) (HM.fromList keyVals)

type Addr = Lab.Label

data Exceptional
    = Break Label Value
    | Throw Value
    deriving (Show, Eq, Generic)


eval ::
  ConcreteT (
    EnvT Ident Value (
      StoreT Addr Value (
        FailureT String (
          ExceptT Exceptional (
            ->)))))
  Expr Value
eval = Shared.eval

run :: [(Ident, Value)] -> [(Addr, Value)] -> Expr -> Error Exceptional (Error String (HashMap Addr Value, Value))
run env st e =
  runExceptT (
    runFailureT (
      runStoreT (
        runEnvT (
          runConcreteT (
            eval)))))
  (HM.fromList st, (HM.fromList env, e))

-- | Arrow transformer that implements the concrete value semantics
newtype ConcreteT c x y = ConcreteT { runConcreteT :: c x y }
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowFail f, ArrowExcept ex, ArrowStore addr val)
deriving instance ArrowFix x y c => ArrowFix x y (ConcreteT c)
deriving instance ArrowEnv var Value env c => ArrowEnv var Value env (ConcreteT c)

instance (ArrowChoice c, Profunctor c) => ArrowAlloc (Lab.Label,Value) Addr (ConcreteT c) where
  alloc = arr $ \(l,_) -> l


instance (ArrowChoice c,
          ArrowFail f c,
          IsString f,
          ArrowStore Addr Value c,
          Store.Join c ((Value, Addr), Addr) Value
         ) => IsVal Value Env (ConcreteT c) where
    -- simple values
    numVal = arr VNumber
    boolVal = arr VBool
    stringVal = arr VString
    undefVal = arr (const VUndefined)
    nullVal = arr (const VNull)
    evalOp = evalOp_

    -- closures
    closureVal = arr $ \(env, ids, body) -> VLambda ids body env
    -- | applies a closure to an argument. The given continuation
    -- describes how to evaluated the body of the closure.
    applyClosure f = proc (fun, args) ->
        case fun of
          VLambda names body closureEnv -> f -< ((closureEnv, names, body), args)
          _ -> fail -< fromString $ printf "Error: apply on non-lambda value: %s %s" (show fun) (show args)

    -- objects
    objectVal = arr objectFromList
    getField = proc (o, f) -> case o of
      VObject _ vals -> case f of
        VString name -> case HM.lookup name vals of
          Just v -> returnA -< v
          Nothing -> case HM.lookup "$proto" vals of
            Just (VRef a) -> do
              protoV <- read' -< a
              getField -< (protoV, f)
            _ -> returnA -< VUndefined
        _ -> failWrongType -< ("String", f)
      _ -> failWrongType -< ("Object", o)
    updateField = proc (o, f, v) -> case o of
      VObject props vals -> case f of
        VString name ->
          if HM.member name vals
            then returnA -< VObject props (HM.insert name v vals)
            else returnA -< VObject (name:props) (HM.insert name v vals)
        _ -> failWrongType -< ("String", f)
      _ -> failWrongType -< ("Object", o)
    deleteField = proc (o, f) -> case o of
      VObject props vals -> case f of
        VString name -> returnA -< VObject (delete name props) (HM.delete name vals)
        _ -> failWrongType -< ("String", f)
      _ -> failWrongType -< ("Object", o)

instance ArrowChoice c => IsRef Value Addr (ConcreteT c) where
  type RefJoin (ConcreteT c) x y = ()

  -- store ops
  ref = arr VRef
  withRef f g = proc (e, v) -> case v of
    VRef a -> f -< (e, (a, v))
    _ -> g -< (e, v)

instance (ArrowChoice c, ArrowExcept Exceptional c) => IsException Value Exceptional (ConcreteT c) where
  type ExcJoin (ConcreteT c) x y = ()

  throwExc = arr Throw
  breakExc = arr (uncurry Break)
  handleThrow f = proc (x, e) -> case e of
    Throw v -> f -< (x, v)
    _ -> throw -< e
  handleBreak f = proc (x, e) -> case e of
    Break l v -> f -< (x, l, v)
    _ -> throw -< e

instance (ArrowChoice c, ArrowFail f c, IsString f) => ArrowCond Value (ConcreteT c) where
  type Join (ConcreteT c) x y = ()

  if_ f1 f2 = proc (cond, (x, y)) -> case cond of
    VBool True  -> f1 -< x
    VBool False -> f2 -< y
    _           -> failWrongType -< ("Bool", cond)


failWrongType :: (ArrowChoice c, ArrowFail f c, IsString f, Show a) => c (String, a) b
failWrongType = proc (typ, a) -> fail -< fromString $ printf "Wrong type. Expected %s but found %s" typ (show a)

evalOp_ :: (ArrowChoice c, ArrowFail f c, IsString f) => c (Op, [Value]) Value
evalOp_ = proc (op, args) -> case (op, args) of
    -- number operators
    (ONumPlus, [(VNumber a), (VNumber b)]) -> returnA -< VNumber (a + b)
    (OMul, [(VNumber a), (VNumber b)]) -> returnA -< VNumber (a * b)
    (ODiv, [(VNumber a), (VNumber b)]) -> returnA -< VNumber (a / b)
    (OMod, [(VNumber a), (VNumber b)]) -> returnA -< VNumber (mod' a b)
    (OSub, [(VNumber a), (VNumber b)]) -> returnA -< VNumber (a - b)
    (OLt, [(VNumber a), (VNumber b)]) -> returnA -< VBool (a < b)
    (OToInteger, [(VNumber a)]) -> returnA -< VNumber $ fromInteger (truncate a)
    (OToInt32, [(VNumber a)]) ->
        returnA -< (let n = mod (truncate a) ((2::Integer)^(32 :: Integer)) in
            if n > ((2::Integer)^(31 :: Integer)) then VNumber $ fromInteger $ n - ((2::Integer)^(32 :: Integer))
            else VNumber $ fromInteger $ n)
    (OToUInt32, [(VNumber a)]) -> returnA -< VNumber $ fromInteger $ mod (abs $ truncate a) ((2::Integer)^(32 :: Integer))
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
        let elems = zip (Prelude.map (show :: Integer -> String) [0..]) (Prelude.map VString $ splitOn delim subject)
        returnA -< objectFromList $ elems ++ [("length", VNumber $ fromIntegral $ length elems), ("$proto", VString "Array")]
    (OStrSplitRegExp, _) -> fail -< fromString $ "Regex operations not implemented"
    (ORegExpMatch, _) -> fail -< fromString $ "Regex operations not implemented"
    (ORegExpQuote, _) -> fail -< fromString $ "Regex operations not implemented"
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
    (OPrimToNum, [a]) -> case a of
        (VNumber n)  -> returnA -< VNumber n
        (VString s)  -> returnA -< VNumber $ (case (readMaybe s :: Maybe Double) of
            Just num -> num
            Nothing  -> 0/0)
        (VBool b)    -> returnA -< if b then VNumber 1.0 else VNumber 0.0
        (VNull)      -> returnA -< VNumber 0
        (VUndefined) -> returnA -< VNumber (0/0)
        _            -> fail    -< fromString $ "Error: unimplemented primToNum for " ++ (show a)
    -- primToStr operator
    (OPrimToStr, [a]) -> case a of
        (VNumber n)   -> returnA -< if (fromInteger $ floor n) == n then (VString $ show $ (floor::Double->Integer) n) else (VString $ show n)
        (VString s)   -> returnA -< VString s
        (VBool b)     -> returnA -< VString $ if b then "true" else "false"
        (VNull)       -> returnA -< VString "null"
        (VUndefined)  -> returnA -< VString "undefined"
        (VObject _ _) -> returnA -< VString "object"
        _             -> fail    -< fromString $ "Error: unimplemented primToStr for " ++ (show a)
    -- primToBool operator
    (OPrimToBool, [a]) -> case a of
        (VNumber n)    -> returnA -< VBool $ (n /= 0.0) && (not (isNaN n))
        (VString s)    -> returnA -< VBool $ not $ s == ""
        (VBool b)      -> returnA -< VBool b
        (VNull)        -> returnA -< VBool False
        (VUndefined)   -> returnA -< VBool False
        (VObject _ _)  -> returnA -< VBool True
        _              -> fail    -< fromString $ "Error: unimplemented primToBool for " ++ (show a)
    -- typeOf operator
    (OTypeof, [a]) -> returnA -< VString (case a of
        (VNumber _)     -> "number"
        (VString _)     -> "string"
        (VBool _)       -> "boolean"
        (VUndefined)    -> "undefined"
        (VNull)         -> "null"
        (VLambda _ _ _) -> "lambda"
        (VObject _ _)   -> "object"
        (VRef _)        -> "location")
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
    (OHasOwnProp, [(VObject _ vals), (VString field)]) ->
        returnA -< VBool $ HM.member field vals
    (OObjCanDelete, [(VObject _ _), (VString field)]) ->
        returnA -< VBool $ (length field) > 0 && (not $ head field == '$')
    (OObjIterHasNext, [(VObject props _), VUndefined]) ->
        returnA -< VBool $ not $ null props
    (OObjIterHasNext, [(VObject props _), (VNumber i)]) ->
        returnA -< VBool $ (floor i)+1 < length props
    (OObjIterNext, [(VObject (_:_) _), VUndefined]) ->
        returnA -< VNumber $ 0
    (OObjIterNext, [(VObject _ _), (VNumber i)]) -> do
        returnA -< VNumber $ i + 1
    (OObjIterKey, [(VObject props _), (VNumber i)]) -> do
        returnA -< (VString $ props !! floor i)
    (OSurfaceTypeof, [a]) -> case a of
        VObject _ props -> returnA -< VString $ if HM.member "$code" props then "function" else "object"
        VNull           -> returnA -< VString "object"
        VUndefined      -> returnA -< VString "undefined"
        VNumber _       -> returnA -< VString "number"
        VString _       -> returnA -< VString "string"
        VBool _         -> returnA -< VString "boolean"
        _               -> fail    -< fromString $ "Error: unimplemented typeOf for " ++ (show a)
    _ -> fail -< fromString $ "Unimplemented operator: " ++ (show op) ++ " with args: " ++ (show args)
