{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module GenericInterpreter where

import           Prelude hiding (succ, pred, fail, map)
import           Syntax (Literal(..), Expr(..), Op1(..), Op2(..), OpVar(..))

import           Control.Arrow
import           Control.Arrow.Fail(ArrowFail,failString)
import qualified Control.Arrow.Fail as Fail
import           Control.Arrow.Fix
import           Control.Arrow.Environment(ArrowEnv)
import qualified Control.Arrow.Environment as Env
import           Control.Arrow.Closure (ArrowClosure)
import qualified Control.Arrow.Closure as Cls
import           Control.Arrow.Store (ArrowStore,write,read')
import qualified Control.Arrow.Store as Store 
import           Control.Arrow.Utils (map)

import           Data.Utils(eqLength)
import           Data.Text (Text)
import           Text.Printf
import           Data.List.Split
import           Data.Label
import           Data.Kind(Type)

import           GHC.Exts (IsString(..),Constraint)

-- | Shared interpreter for Scheme.
eval :: (ArrowChoice c,
         ArrowFix (c [Expr] v),
         ArrowEnv Text addr c,
         ArrowStore addr v c,
         ArrowFail e c,
         IsString e,
         ArrowClosure Expr v c,
         IsVal v c,
         Env.Join v c,
         Cls.Join v v c,
         Store.Join v c,
         Fail.Join v c,
         Join v c,
         Show addr,
         ArrowAlloc addr c,
         Env.Join addr c)
     => c [Expr] v -> c Expr v
eval run' = proc e0 -> case e0 of
  Lit x _ -> lit -< x
  Nil l -> nil_ -< l
  Cons x xs _ -> do
    v <- run' -< [x]
    vs <- run' -< [xs]
    cons_ -< ((v,label x),(vs, label xs))
  Begin es _ ->
    run' -< es
  App e1 e2 _ -> do
    fun <- run' -< [e1]
    args <- map run' -< chunksOf 1 e2
    Cls.apply applyClosure' -< (fun, args)
  Apply es _ -> run' -< es
  -- Scheme expression
  Var x _ -> Env.lookup'' read' -< x
  Lam xs es l -> Cls.closure -< Lam xs es l
  Let bnds body l -> do -- iterative evaluation of bindings really necessary?
    vs <- evalBindings -< (bnds,l)
    Env.extend' run' -< (vs,body)
  LetRec bnds body l -> do
    addrs <- map alloc -< [(var,l) | (var,_) <- bnds]
    let envbnds = [(var,addr) | ((var,_),addr) <- zip bnds addrs]
    let storebnds = [(addr,expr) | ((_,expr),addr) <- zip bnds addrs]
    Env.extend' evalBindings' -< (envbnds,(storebnds,body))
  Set x e l -> run' -< [Set x e l]
  Define xs e l -> run' -< [Define xs e l]
  If e1 e2 e3 _ -> do
    v1 <- run' -< [e1]
    if_ run' run' -< (v1, ([e2], [e3]))
  -- -- Scheme standard procedures
  Op1 x e1 _ -> do
    v1 <- run' -< [e1]
    op1_ -< (x,v1)
  Op2 x e1 e2 _ -> do
    v1 <- run' -< [e1]
    v2 <- run' -< [e2]
    op2_ -< (x, v1, v2)
  OpVar x es _ -> do
    vs <- map run' -< chunksOf 1 es
    opvar_ -< (x, vs)
  Error err _ ->
    failString -< "error: " ++ err
  Breakpoint e ->
    run' -< [e]
  where
    -- Helper function used to apply closure or a suspended fixpoint computation to its argument.
    applyClosure' = proc (e, args) -> case e of  -- args = [(argVal, argLabel)]
      Lam xs body l ->
        if eqLength xs args
          then do
            addrs <- map alloc -< [ (x,l) | x <- xs]
            map write -< zip addrs args
            Env.extend' run' -< (zip xs addrs, [Apply body l])
          else
            failString -< printf "Applied the function %s with %d arguments to %d arguments" (show e) (length xs) (length args)
      _ -> failString -< printf "Expected a function, but got %s" (show e)
    {-# SCC applyClosure' #-}

    evalBindings = proc (bnds,lab) -> case bnds of
      [] -> returnA -< []
      (var,expr) : bnds' -> do
        val <- run' -< [expr]
        addr <- alloc -< (var,lab)
        write -< (addr,val)
        vs <- evalBindings -< (bnds',lab) 
        returnA -< (var,addr) : vs
    {-# SCC evalBindings #-}

    evalBindings' = proc (bnds,body) -> case bnds of
      [] -> run' -< body
      (addr,expr) : bnds' -> do
        val <- run' -< [expr]
        write -< (addr,val)
        evalBindings' -< (bnds',body)
{-# INLINEABLE eval #-}
{-# SCC eval #-}

run :: (ArrowChoice c,
        ArrowEnv Text addr c,
        ArrowStore addr v c,
        ArrowFail e c,
        IsString e,
        ArrowClosure Expr v c,
        IsVal v c,
        Env.Join v c,
        Env.Join addr c,
        Cls.Join v v c,
        Store.Join v c,
        Fail.Join v c,
        Fail.Join addr c,
        Join v c,
        ArrowAlloc addr c,
        Show addr)
    => c Expr v -> c [Expr] v -> c [Expr] v
run eval' run' = proc es -> case es of
  Set x e _:rest -> do
    v <- run' -< [e]
    addr <- Env.lookup (proc (addr, _) -> returnA -< addr)
               (proc var -> failString -< printf "(set!): cannot set variable %s before its definition" (show var))
                 -< (x, x)
    write -< (addr,v)
    run' -< rest
  Define x e l: rest -> do
    addr <- alloc -< (x,l)
    Env.extend (proc (e,addr,rest) -> do
        val <- run' -< [e]
        write -< (addr,val)
        run' -< rest
      ) -< (x,addr, (e,addr,rest))
  [] ->
    void -< ()
  e:[] ->
    eval' -< e
  e:rest -> do
    eval' -< e
    run' -< rest
{-# INLINEABLE run #-}
{-# SCC run #-}

runFixed :: (
  ?fixpointAlgorithm :: FixpointAlgorithm (Fix (c [Expr] v)),
  ArrowFix (c [Expr] v),
  ArrowChoice c,
  ArrowEnv Text addr c,
  ArrowStore addr v c,
  ArrowFail e c,
  IsString e,
  ArrowClosure Expr v c,
  IsVal v c,
  Env.Join v c,
  Env.Join addr c,
  Cls.Join v v c,
  Store.Join v c,
  Fail.Join v c,
  Fail.Join addr c,
  Join v c,
  ArrowAlloc addr c,
  Show addr
  ) =>
  c [Expr] v
runFixed = fix $ \run' -> run (eval run') run'
{-# INLINE runFixed #-}

class ArrowAlloc addr c where
  alloc :: c (Text,Label) addr

class (Arrow c) => IsVal v c | c -> v where
  type family Join y (c :: Type -> Type -> Type) :: Constraint
  lit :: c Literal v
  if_ :: Join z c => c x z -> c y z -> c (v, (x, y)) z

  void :: c () v
  nil_ :: c Label v
  cons_ :: c ((v, Label), (v, Label)) v

  op1_ :: c (Op1, v) v
  op2_ :: c (Op2, v, v) v
  opvar_ :: c (OpVar, [v]) v
