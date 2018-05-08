{-# LANGUAGE Arrows #-}
module TypeInterpreter where

import Syntax

import Control.Category
import Control.Arrow

import Data.Map (Map, findWithDefault)
import qualified Data.Map as Map

newtype LJSArrow x y = LJSArrow {runArr :: x -> Store -> Either String (Store, y) } 

instance Category LJSArrow where
    id = LJSArrow (\x st -> Right (st, x))
    LJSArrow f . LJSArrow g = LJSArrow $ \x st -> case g x st of
        Left er -> Left er
        Right (st', y) -> f y st'

instance Arrow LJSArrow where
    arr f = LJSArrow (\x st -> Right (st, f x))
    first (LJSArrow f) = LJSArrow $ \(x, y) st -> case f x st of
        Left er -> Left er
        Right (st', z) -> Right (st', (z, y))

instance ArrowChoice LJSArrow where
    left (LJSArrow f) = LJSArrow $ \e st -> case e of
        Left x -> case f x st of
            Left err -> Left err
            Right (st', y) -> Right (st', Left y)
        Right z -> Right (st, Right z)

-- Constructs an arrow that operates on a list of inputs producing a list of outputs.
-- Used for evaluating a list of arguments, producing a list of values which are passed to the evalOp function.
mapA :: ArrowChoice arr => arr a b -> arr [a] [b]
mapA f = arr (\list -> case list of 
    [] -> Left ()
    (x : xs) -> Right (x, xs)) >>> (arr (const []) ||| ((f *** mapA f) >>> (arr (uncurry (:)))))

type Store = Map String Type
lookup :: String -> Store -> Maybe Type
lookup s st = Map.lookup s st 
empty :: Store
empty = Map.empty
insert :: String -> Type -> Store -> Store
insert s t st = Map.insert s t st

get :: LJSArrow () Store
get = LJSArrow (\() st -> Right (st, st))

put :: LJSArrow Store ()
put = LJSArrow (\st _ -> Right (st, ()))

throw :: LJSArrow String a
throw = LJSArrow (\er _ -> Left er)

evalOp :: LJSArrow (Op, [Expr]) Type
evalOp = proc (op, exps) -> case op of
    ONumPlus -> returnA -< TNumber
    OMul -> returnA -< TNumber
    ODiv -> returnA -< TNumber
    OMod -> returnA -< TNumber
    OSub -> returnA -< TNumber
    OLt -> returnA -< TBool
    OToInteger -> returnA -< TNumber
    OToInt32 -> returnA -< TNumber
    OToUInt32 -> returnA -< TNumber
    OLShift -> returnA -< TNumber
    OSpRShift -> returnA -< TNumber
    OZfRShift -> returnA -< TNumber

    OStrPlus -> returnA -< TString
    OStrLt -> returnA -< TBool
    OStrLen -> returnA -< TNumber
    OStrStartsWith -> returnA -< TBool

    OBAnd -> returnA -< TBool
    OBOr -> returnA -< TBool
    OBXOr -> returnA -< TBool
    OBNot -> returnA -< TBool

    OIsPrim -> returnA -< TBool
    OPrimToNum -> returnA -< TNumber
    OPrimToStr -> returnA -< TString
    OPrimToBool -> returnA -< TBool
    OTypeof -> returnA -< TString

    OStrictEq -> returnA -< TBool
    OAbstractEq -> returnA -< TBool
    OMathExp -> returnA -< TNumber
    OMathLog -> returnA -< TNumber
    OMathCos -> returnA -< TNumber
    OMathSin -> returnA -< TNumber
    OMathAbs -> returnA -< TNumber
    OMathPow -> returnA -< TNumber

    OHasOwnProp -> returnA -< TBool

eval :: LJSArrow Expr Type
eval = proc e -> case e of
    ENumber _ -> returnA -< TNumber
    EString _ -> returnA -< TString
    EBool _ -> returnA -< TBool
    EUndefined -> returnA -< TUndefined
    ENull -> returnA -< TNull
    ELambda ids exp -> do
        bodyT <- eval -< exp
        returnA -< TLambda ids bodyT
    EObject fields -> do
        types <- (mapA $ second eval) -< fields
        returnA -< TObject types
    EId id -> do
        -- #todo type map instead of value map
        st <- get -< ()
        case Map.lookup id st of
            Just t -> returnA -< t
            Nothing -> returnA -< TUndefined
    EOp op exps -> do
        res <- evalOp -< (op, exps)
        returnA -< res
    EApp (ELambda params body) args -> do
        types <- mapA eval -< args
        zipped <- arr $ uncurry zip -< (params, types)

        scope <- get -< ()
        put -< Map.fromList $ (Map.toList scope) ++ (zipped)

        res <- eval -< body 
        returnA -< res
    ELet vars body -> do
        res <- eval -< EApp (ELambda (map fst vars) body) (map snd vars)
        returnA -< res
    EIf cond thenBranch elseBranch -> do 
        testT <- eval -< cond
        case testT of
            TBool -> do
                thenT <- eval -< thenBranch
                elseT <- eval -< thenBranch
                case thenT == elseT of
                    True -> returnA -< thenT
                    False -> throw -< "Type Error: branches must be of the same type"
            _ -> throw -< "Type Error: if condition must be of type bool"
    -- EGetField objE fieldE -> -- #todo problem: calculated/dynamic field names
    -- EUpdateField objE nameE fieldE -> -- #todo problem: calculated/dynamic field names
    -- EDeleteField objE nameE -> -- #todo problem: calculated/dynamic field names
    ESeq f s -> do
        -- #todo Consider breaks/throws in f?
        res <- eval -< s
        returnA -< res
    -- #todo Consider breaks/throws in b?
    EWhile _ _ -> returnA -< TUndefined 
    -- EEval -> -- #todo Bottom type? Error?
    -- ELabel l e -> -- #todo ?
    -- EBreak l e -> -- #todo ?
    -- EThrow e -> -- #todo ?
    -- ECatch try catch -> -- #todo ?
    -- EFinally b1 b2 -> -- #todo ?
