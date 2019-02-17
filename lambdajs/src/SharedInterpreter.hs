{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}

{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module SharedInterpreter where

import           Prelude             hiding (break, lookup, map, read, fail)
import qualified Prelude

import           Control.Arrow
import           Control.Arrow.Alloc
import           Control.Arrow.Environment as Env
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Store as Store
import           Control.Arrow.Utils (map)

import qualified Data.Label as Lab
import           Data.String

import           Text.Printf

import           Syntax

-- TODO
-- This typeclass can be split up into multiple to partially reuse implementations across interpreters
-- But this might not save effort if the implementation is completely different.

class Arrow c => JSOps v env addr c | c -> v, c -> env, c -> addr where
    -- simple values
    numVal :: c Double v
    boolVal :: c Bool v
    stringVal :: c String v
    undefVal :: c () v
    nullVal :: c () v
    evalOp :: c (Op, [v]) v

    -- closures
    closureVal :: c (env, [Ident], Expr) v
    -- | applies a closure to an argument. The given continuation
    -- describes how to evaluated the body of the closure.
    applyClosure :: c ((env, [Ident], Expr),[v]) v -> c (v, [v]) v

    -- objects
    objectVal :: c [(Ident, v)] v
    getField :: c Expr v -> c (v, Expr) v
    updateField :: c Expr v -> c (v, Expr, v) v
    deleteField :: c Expr v -> c (v, Expr) v

    -- store ops
    ref :: c addr v
    withRef :: c (e,(addr,v)) x -> c (e,v) x -> c (e,v) x 

    -- control flow
    if_ :: c Expr v -> c Expr v -> c (v, Expr, Expr) v
    label :: c Expr v -> c (Label, Expr) v
    break :: c (Label, v) v
    catch :: c Expr v -> c (Expr, Expr) v
    throw :: c v v

withRef' :: (JSOps v env addr c, ArrowFail String c, Show v) => c (e,(addr,v)) x -> c (e,v) x 
withRef' f = withRef f (proc (_,refVal) -> fail -< printf "Not a reference %s" (show refVal))


eval :: (JSOps v env addr c, ArrowChoice c, ArrowFix Expr v c,
         ArrowEnv Ident v env c, ArrowFail String c,
         ArrowStore addr v c, ArrowAlloc (Lab.Label,v) addr c,
         Show v, Show addr,
         Env.Join c ((v,Ident),Ident) v, Store.Join c ((v, addr), addr) v
        ) => c Expr v
eval = fix $ \ev -> proc e -> do
    case e of
        -- simple value expressions
        ENumber d -> numVal -< d
        EString s -> stringVal -< s
        EBool b -> boolVal -< b
        EUndefined -> undefVal -< ()
        ENull -> nullVal -< ()
        EOp op exps -> do
            vals <- (map ev) -< exps
            evalOp -< (op, vals)

        -- closure expressions
        ELambda ids ex -> do
            env <- getEnv -< ()
            closureVal -< (env, ids, ex)
        EApp fun args -> do
            funVal <- ev -< fun
            argVals <- map ev -< args
            (applyClosure $ evalBody ev) -< (funVal, argVals)

        -- object eexpressions
        EObject fields -> do
            vals <- map (second ev) -< fields
            objectVal -< vals
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

        -- environment related expressions
        EId ident -> lookup' -< ident
        ELet varArgs body -> do
            varVals <- map (second ev) -< varArgs
            env <- getEnv -< ()
            env' <- bindings -< (varVals, env)
            localEnv ev -< (env', body)

        -- store related expressions
        ERef lab exp -> do
            val <- ev -< exp
            addr <- alloc -< (lab, val)
            write -< (addr, val)
            ref -< addr
        EDeref exp -> do
            refVal <- ev -< exp
            withRef' (proc (_, (a, _)) -> read' -< a) -< ((), refVal)
        ESetRef lhs rhs -> do
            refVal <- ev -< lhs
            rhsVal <- ev -< rhs
            withRef' (proc (rhsVal, (a, refVal)) -> do
                        write -< (a, rhsVal)
                        returnA -< refVal)
                     -< (rhsVal,refVal)

        -- control related expressions
        ESeq e1 e2 -> do
            ev -< e1
            ev -< e2
        EIf condE thenE elseE -> do
            cond <- ev -< condE
            if_ ev ev -< (cond, thenE, elseE)
        EWhile cond body -> do
            ev -< EIf cond (ESeq body e) EUndefined
        ELabel l ex -> do
            label ev -< (l, ex)
        EBreak l ex -> do
            val <- ev -< ex
            break -< (l, val)

        -- exception related expressions
        EThrow ex -> do
            val <- ev -< ex
            throw -< val
        ECatch try catchE -> do
            catch ev -< (try, catchE)
        EFinally e1 e2 -> do
            res <- ev -< e1
            ev -< e2
            returnA -< res

        -- self-eval expression
        EEval -> fail -< "Encountered EEval"

    where
      evalBody ev = proc ((env, vars, body), argVals) -> do
        env' <- bindings -< (zip vars argVals, env)
        localEnv ev -< (env', body)
