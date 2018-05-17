{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
module ValueInterpreter where
import Prelude hiding(lookup, read)
import qualified Prelude(read)

import Syntax

import Data.Concrete.Error
import Data.Concrete.Store
import Data.Concrete.Environment
import Control.Category
import Control.Arrow
import Control.Arrow.Transformer.Concrete.Except
import Control.Arrow.Transformer.Concrete.Environment
import Control.Arrow.Transformer.Concrete.Store
import Control.Arrow.Transformer.Const
import Control.Arrow.Transformer.Static
import Control.Arrow.Fail
import Control.Arrow.Try
import Control.Arrow.Const
import Control.Arrow.Environment
import Control.Arrow.State
import Control.Arrow.Transformer.State
import Control.Arrow.Store
import Control.Arrow.Reader
import Control.Arrow.Utils (foldA)
import Control.Arrow.TryCatch
import Data.Identifiable
import Data.Fixed (mod')
import Data.List (isPrefixOf, find)
import Data.Bits (shift, bit)
import Data.Word (Word32)

import Data.Map (Map, findWithDefault)
import qualified Data.Map as Map

newtype LJSArrow x y = LJSArrow (Except (Either String Exceptional) (Environment Ident Location (StoreArrow Location Value (State Location (->)))) x y)
    deriving (ArrowChoice,Arrow,Category)
deriving instance ArrowFail (Either String Exceptional) LJSArrow
deriving instance ArrowEnv Ident Location (Env Ident Location) LJSArrow
deriving instance ArrowApply LJSArrow
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

instance ArrowTryCatch (Either String Exceptional) Expr Value LJSArrow where
    tryCatchA (LJSArrow f) (LJSArrow g) = LJSArrow $ tryCatchA f g

runLJS :: LJSArrow x y -> [(Ident, Location)] -> [(Location, Value)] -> x -> (Location, (Store Location Value, Error (Either String Exceptional) y))
runLJS (LJSArrow f) env env2 x = runState (runStore (runEnvironment (runExcept f))) (Location 0, (Data.Concrete.Store.fromList env2, (env, x)))

runConcrete :: [(Ident, Location)] -> [(Location, Value)] -> Expr -> (Store Location Value, Error (Either String Exceptional) Value)
runConcrete env st exp = case runLJS eval env st exp of
    (l, (st, Fail e)) -> (st, Fail e)
    (l, (st, Success res)) -> (st, Success res)

fresh :: ArrowState Location c => c () Location 
fresh = proc () -> do
    Location s <- getA -< ()
    putA -< Location $ s + 1
    returnA -< Location $ s + 1

-- Constructs an arrow that operates on a list of inputs producing a list of outputs.
-- Used for evaluating a list of arguments, producing a list of values which are passed to the evalOp function.
mapA :: ArrowChoice arr => arr a b -> arr [a] [b]
mapA f = arr (\list -> case list of 
    [] -> Left ()
    (x : xs) -> Right (x, xs)) >>> (arr (const []) ||| ((f *** mapA f) >>> (arr (uncurry (:)))))

evalOp :: (ArrowFail (Either String Exceptional) c, ArrowChoice c) => c (Op, [Value]) Value
evalOp = proc (op, vals) -> case (op, vals) of
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
        res <- evalOp -< (OPrimToNum, [VString b])
        returnA -< VBool $ (VNumber a) == res
    (OAbstractEq, [(VString a), (VNumber b)]) -> do
        res <- evalOp -< (OPrimToNum, [VString a])
        returnA -< VBool $ (VNumber b) == res
    (OAbstractEq, [(VBool a), (VNumber b)]) -> do
        res <- evalOp -< (OPrimToNum, [VBool a])
        returnA -< VBool $ (VNumber b) == res
    (OAbstractEq, [(VNumber a), (VBool b)]) -> do
        res <- evalOp -< (OPrimToNum, [VBool b])
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

getField :: (ArrowFail (Either String Exceptional) c, ArrowChoice c) => c (Value, Value) Value
getField = proc (VObject fields, VString fieldName) -> 
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
                            res <- getField -< (pv, VString fieldName)
                            returnA -< res
                        -- E-GetField-NotFound
                        Nothing -> returnA -< VUndefined

deleteField :: ArrowFail (Either String Exceptional) e => e (Value, Value) Value
deleteField = proc (VObject fields, VString name) -> do
    filtered <- arr (\(n, fs) -> filter (\(fn, _) -> fn /= n) fs) -< (name, fields) 
    returnA -< VObject filtered

updateField :: (ArrowFail (Either String Exceptional) e, ArrowChoice e) => e (Value, Value, Value) Value
updateField = proc (VObject fields, VString name, value) -> do
    -- remove field from obj
    filtered <- deleteField -< (VObject fields, VString name)
    case filtered of
        VObject obj -> do
            -- add field with new value to obj
            newFields <- arr (\(fs, n, v) -> (n, v) : fs) -< (obj, name, value) 
            returnA -< VObject newFields
        _ -> failA -< Left "Error: deleteField returned non-object value"


eval :: (ArrowFail (Either String Exceptional) c, 
    ArrowState Location c,
    ArrowApply c,
    ArrowConst Label (Static ((->) Label) c),
    ArrowEnv Ident Location (Env Ident Location) c, 
    ArrowStore Location Value () c, ArrowChoice c,
    ArrowTryCatch (Either String Exceptional) Expr Value c) => c Expr Value 
eval = proc e -> case e of
    ENumber d -> returnA -< VNumber d
    EString s -> returnA -< VString s
    EBool b -> returnA -< VBool b
    EUndefined -> returnA -< VUndefined
    ENull -> returnA -< VNull
    ELambda ids exp -> returnA -< VLambda ids exp
    EObject fields -> do
        vals <- (mapA $ second eval) -< fields
        returnA -< VObject vals
    EId id -> do
        loc <- Control.Arrow.Environment.lookup -< id
        returnA -< VRef loc
    EOp op exps -> do
        vals <- mapA eval -< exps
        res <- evalOp -< (op, vals)
        returnA -< res
    EApp (ELambda params body) args -> do
        locations <- (mapA ((arr $ const ()) >>> fresh)) -< [1..(length args)]

        vals <- mapA eval -< args
        forStore <- arr $ uncurry zip -< (locations, vals)
        mapA write -< map (\(a, b) -> (a, b, ())) forStore

        forEnv <- arr $ uncurry zip -< (params, locations)
        scope <- getEnv -< ()
        env' <- bindings -< (forEnv, scope)
        res <- localEnv eval -< (env', body)

        returnA -< res
    ELet vars body -> do
        res <- eval -< EApp (ELambda (map fst vars) body) (map snd vars)
        returnA -< res
    EIf cond thenBranch elseBranch -> do
        testRes <- eval -< cond
        case testRes of
            VBool True -> do
                res <- eval -< thenBranch
                returnA -< res
            VBool False -> do
                res <- eval -< elseBranch
                returnA -< res
    EGetField objE fieldE -> do
        fieldV <- eval -< fieldE
        objV <- eval -< objE
        res <- getField -< (objV, fieldV)
        returnA -< res
    EUpdateField objE nameE fieldE -> do
        nameV <- eval -< nameE
        fieldV <- eval -< fieldE
        objV <- eval -< objE
        res <- updateField -< (objV, nameV, fieldV)
        returnA -< res
    EDeleteField objE nameE -> do
        objV <- eval -< objE
        nameV <- eval -< nameE
        res <- deleteField -< (objV, nameV)
        returnA -< res
    ESeq f s -> do
        res1 <- eval -< f
        res2 <- eval -< s
        returnA -< res2
    EWhile cond body -> do
        condV <- eval -< cond
        case condV of
            VBool True -> do
                eval -< body
                res <- eval -< (EWhile cond body) 
                returnA -< res
            VBool False ->
                returnA -< VUndefined
            _ -> failA -< Left "Error: Non bool value in test of while loop"
    ESetRef locE valE -> do
        loc <- eval -< locE
        val <- eval -< valE
        case loc of
            VRef l -> do
                write -< (l, val, ())
                returnA -< VUndefined
            _ -> failA -< Left "Error: ESetRef lhs did not evaluate to a location"
    ERef valE -> do
        val <- eval -< valE
    
        loc <- fresh -< ()
        write -< (loc, val, ())
        returnA -< VRef loc
    EDeref locE -> do
        loc <- eval -< locE
        case loc of 
            VRef l -> do
                val <- read -< (l, ())
                returnA -< val
            _ -> failA -< Left "Error: EDeref lhs did not evaluate to a location"
    EEval -> failA -< Left "Eval expression encountered, aborting"
    ELabel l e -> do
        res <- app -< (runConst l (tryCatchA eval (proc x -> case x of 
                Left s -> failA -< Left s
                Right (Break l1 v) -> do
                    l <- askConst -< ()
                    case l1 == l of
                        True -> returnA -< v
                        False -> failA -< (Right $ Break l1 v)
                Right (Thrown v) -> failA -< (Right $ Thrown v))), e)
        returnA -< res
    EBreak l e -> do
        res <- eval -< e
        failA -< Right (Break l res)
    EThrow e -> do
        res <- eval -< e
        failA -< Right (Thrown res)
    ECatch try catch -> do
        -- todo
        returnA -< VUndefined
        --  VThrown v -> do
                --case catch of
                    --ELambda [x] body -> do
                        --scope <- getEnv -< ()
                        --loc <- fresh -< ()
                        --env' <- extendEnv -< (x, loc, scope)
                        --write -< (loc, v, ())
                        --res <- localEnv eval -< (env', body)
                        --returnA -< res
                    --_ -> failA -< "Error: Catch block must be of type ELambda"
    EFinally b1 b2 -> do
        res1 <- eval -< b1
        res2 <- eval -< b2
        returnA -< res1
