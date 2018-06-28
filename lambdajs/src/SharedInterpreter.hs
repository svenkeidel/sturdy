{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}

{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module SharedInterpreter where

import           Control.Arrow
import           Control.Arrow.Utils (fold, map, pi1, pi2)
import           Debug.Trace         (trace)
import           Prelude             hiding (break, error, fold, lookup, map,
                                      read)
import qualified Prelude
import           Syntax

class Arrow c => AbstractValue v c | c -> v where
    -- values
    numVal :: c Double v
    boolVal :: c Bool v
    stringVal :: c String v
    undefVal :: c () v
    nullVal :: c () v
    lambdaVal :: c ([Ident], Expr) v
    objectVal :: c [(Ident, v)] v
    getField :: c Expr v -> c (v, Expr) v
    updateField :: c Expr v -> c (v, Expr, v) v
    deleteField :: c Expr v -> c (v, Expr) v
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
    while_ :: c Expr v -> c Expr v -> c (Expr, Expr) v
    label :: c Expr v -> c (Label, Expr) v
    break :: c (Label, v) v
    catch :: c Expr v -> c (Expr, Expr) v
    throw :: c v v
    -- exceptional
    error :: c String v

eval :: (ArrowChoice c, AbstractValue v c, Show v) => c Expr v
eval = proc e -> do
    case e of
        ENumber d -> do
            numVal -< d
        EString s -> do
            stringVal -< s
        EBool b -> do
            boolVal -< b
        EUndefined -> do
            undefVal -< ()
        ENull -> do
            nullVal -< ()
        ELambda ids exp -> do
            lambdaVal -< (ids, exp)
        EObject fields -> do
            vals <- (Control.Arrow.Utils.map $ second eval) -< fields
            objectVal -< vals
        EId id -> SharedInterpreter.lookup -< id
        EOp op exps -> do
            vals <- (map eval) -< exps
            evalOp -< (op, vals)
        EApp body args -> do
            lambda <- eval -< body
            args <- map eval -< args
            apply eval -< (lambda, args)
        ELet argsE body -> do
            eval -< EApp (ELambda (Prelude.map fst argsE) body) (Prelude.map snd argsE)
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
            getField eval -< (obj, fieldE)
        EUpdateField objE fieldE valE -> do
            val <- eval -< valE
            obj <- eval -< objE
            updateField eval -< (obj, fieldE, val)
        EDeleteField objE fieldE -> do
            obj <- eval -< objE
            deleteField eval -< (obj, fieldE)
        ESeq one two -> do
            eval -< one
            eval -< two
        EIf condE thenE elseE -> do
            cond <- eval -< condE
            if_ eval eval -< (cond, thenE, elseE)
        EWhile cond body -> do
            while_ eval eval -< (cond, body)
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
