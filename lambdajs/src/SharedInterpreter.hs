{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module SharedInterpreter where

import Prelude hiding(lookup, break, read, error)
import qualified Prelude
import Syntax
import Control.Arrow
import Control.Arrow.Utils (foldA, mapA, pi2, pi1)

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
    apply :: c Expr v -> c (v, [v]) v
    -- store ops
    set :: c (v, v) ()
    new :: c v v
    get :: c v v
    -- control flow
    if_ :: c Expr v -> c Expr v -> c (v, Expr, Expr) v
    label :: c Expr v -> c (Label, Expr) v
    break :: c (Label, v) v
    catch :: c Expr v -> c (Expr, Expr) v
    throw :: c v v
    -- exceptional
    error :: c String v

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
    EApp body args -> do
        lambda <- eval -< body
        args <- mapA eval -< args 
        apply eval -< (lambda, args)
    ELet argsE body -> do
        eval -< EApp (ELambda (map fst argsE) body) (map snd argsE)
    ESetRef locE valE -> do
        loc <- eval -< locE
        val <- eval -< valE
        set -< (loc, val)
        returnA -< loc
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
        if_ eval eval -< (cond, thenE, elseE)
    EWhile cond body -> do
        eval -< EIf cond (ESeq body (EWhile cond body)) (EUndefined)
    ELabel l e -> do
        label eval -< (l, e)
    EBreak l e -> do
        val <- eval -< e
        break -< (l, val)
    EThrow e -> do
        val <- eval -< e
        throw -< val
    ECatch try catchE -> do
        catch eval -< (try, catchE)
    EFinally e1 e2 -> do
        res <- eval -< e1
        eval -< e2
        returnA -< res
    EEval -> error -< "Encountered EEval"
