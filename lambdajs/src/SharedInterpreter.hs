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
import           Control.Arrow.Fix
import           Control.Arrow.Utils (map)
import           Prelude             hiding (break, error, lookup, map, read)
import qualified Prelude
import           Syntax

-- TODO
-- This typeclass can be split up into multiple to partially reuse implementations across interpreters
-- But this might not save effort if the implementation is completely different.

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

eval :: (ArrowChoice c, ArrowFix Expr v c, AbstractValue v c, Show v) => c Expr v
eval = fix $ \ev -> proc e -> do
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
        ELambda ids ex -> do
            lambdaVal -< (ids, ex)
        EObject fields -> do
            vals <- (Control.Arrow.Utils.map $ second ev) -< fields
            objectVal -< vals
        EId ident -> SharedInterpreter.lookup -< ident
        EOp op exps -> do
            vals <- (map ev) -< exps
            evalOp -< (op, vals)
        EApp body argsE -> do
            lambda <- ev -< body
            args <- map ev -< argsE
            apply ev -< (lambda, args)
        ELet argsE body -> do
            ev -< EApp (ELambda (Prelude.map fst argsE) body) (Prelude.map snd argsE)
        ESetRef locE valE -> do
            loc <- ev -< locE
            val <- ev -< valE
            set -< (loc, val)
            returnA -< loc
        ERef valE -> do
            val <- ev -< valE
            new -< val
        EDeref locE -> do
            loc <- ev -< locE
            get -< loc
        EGetField objE fieldE -> do
            obj <- ev -< objE
            getField ev -< (obj, fieldE)
        EUpdateField objE fieldE valE -> do
            val <- ev -< valE
            obj <- ev -< objE
            updateField ev -< (obj, fieldE, val)
        EDeleteField objE fieldE -> do
            obj <- ev -< objE
            deleteField ev -< (obj, fieldE)
        ESeq one two -> do
            ev -< one
            ev -< two
        EIf condE thenE elseE -> do
            cond <- ev -< condE
            if_ ev ev -< (cond, thenE, elseE)
        EWhile cond body -> do
            while_ ev ev -< (cond, body)
        ELabel l ex -> do
            label ev -< (l, ex)
        EBreak l ex -> do
            val <- ev -< ex
            break -< (l, val)
        EThrow ex -> do
            val <- ev -< ex
            throw -< val
        ECatch try catchE -> do
            catch ev -< (try, catchE)
        EFinally e1 e2 -> do
            res <- ev -< e1
            ev -< e2
            returnA -< res
        EEval -> error -< "Encountered EEval"
