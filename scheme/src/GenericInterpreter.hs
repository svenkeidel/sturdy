{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module GenericInterpreter where

import           Prelude hiding (succ, pred, fail, map)
import           Syntax (Literal(..), Expr(..), Op1(..), Op2(..), OpVar(..),Op1List(..))

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
import           Control.Arrow.Order 
import           Control.Arrow.Fix.GarbageCollection (ArrowGarbageCollection)
import qualified Control.Arrow.Fix.GarbageCollection as GC 

import           Data.Utils(eqLength)
import           Data.Text (Text)
import           Text.Printf
import           Data.List.Split
import           Data.Label
import           Data.Kind(Type)
import           Data.HashSet (toList,fromList,empty,union,insert)
import           Data.Hashable (Hashable)

import           GHC.Exts (IsString(..),Constraint)

-- | Shared interpreter for Scheme.
eval :: (ArrowChoice c,
         ArrowEnv Text addr c,
         ArrowStore addr v c,
         ArrowFail e c,
         IsString e,
         ArrowClosure Expr v c,
         IsVal v c,
         IsList_ v c,
         Env.Join v c,
         Cls.Join v v c,
         Store.Join v c,
         Fail.Join v c,
         Join v c,
         Show addr,
         ArrowAlloc addr c,
         ArrowGarbageCollection v addr c, 
         Eq addr, 
         Hashable addr)
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
    ret <- evalApp -< (fun,[],e2)
    rootAddrs <- GC.getGCRoots -< () 
    -- failString -< show rootAddrs

    collectAddrs -< ret
  Apply es _ -> run' -< es
  -- Scheme expression
  Var x _ -> Env.lookup'' read' -< x
  Lam xs es l -> Cls.closure -< Lam xs es l
  Let bnds body l -> do
    evalLet -< ([],bnds,body,l) 
  LetRec bnds body l -> do
    addrs <- map alloc -< [(var,l) | (var,_) <- bnds]
    let envbnds = [(var,addr) | ((var,_),addr) <- zip bnds addrs]
    let storebnds = [(addr,expr) | ((_,expr),addr) <- zip bnds addrs]
    extendAddrs evalLetRec -< (envbnds,(storebnds,body))
  Set x e l -> run' -< [Set x e l]
  Define xs e l -> run' -< [Define xs e l]
  If e1 e2 e3 _ -> do
    v1 <- run' -< [e1]
    if_ run' run' -< (v1, ([e2], [e3]))
  -- -- Scheme standard procedures
  Op1 x e1 _ -> do
    v1 <- run' -< [e1]
    op1_ -< (x,v1)
  Op1List x e1 _ -> do
    v1 <- run' -< [e1]
    op1list_ -< (x,v1)
  Op2 x e1 e2 _ -> do
    v1 <- run' -< [e1]
    v2 <- run' -< [e2]
    op2_ -< (x, v1, v2)
  OpVar x es _ -> do
    vs <- map run' -< chunksOf 1 es
    opvar_ -< (x, vs)
  Error err _ ->
    failString -< "error: " ++ err
  where
    
    evalLet = proc (bnds_env,bnds,body,lab) -> case bnds of 
      [] -> extendAddrs run' -< (bnds_env, body) 
      ((var,expr) : bnds_) -> do
        val <- run' -< [expr] 
        addr <- alloc -< (var,lab)
        write -< (addr,val)  
        addr_val <- GC.getAddrVal -< val 
        GC.addLocalGCRoots evalLet -< (insert addr addr_val,(bnds_env ++ [(var,addr)],bnds_,body,lab))
    {-# SCC evalLet #-}

    evalLetRec = proc (bnds,body) -> case bnds of
      [] -> run' -< body
      (addr,expr) : bnds' -> do
        val <- run' -< [expr]
        write -< (addr,val)
        evalLetRec -< (bnds',body)
    {-# SCC evalLetRec #-}

    evalApp = proc (fun, argsv, args) -> case args of 
      [] -> Cls.apply applyClosure -< (fun, argsv) 
      (arg:args_) -> do 
        val <- run' -< [arg] 
        addrs <- GC.getAddrVal -< val
        GC.addLocalGCRoots evalApp -< (addrs, (fun, argsv ++ [val], args_))
    {-# SCC evalApp #-}

    -- Helper function used to apply closure or a suspended fixpoint computation to its argument.
    applyClosure = proc (e, args) -> case e of  -- args = [(argVal, argLabel)]
      Lam xs body l ->
        if eqLength xs args
          then do
            addrs <- map alloc -< [ (x,l) | x <- xs]
            map write -< zip addrs args
            extendAddrs run' -< (zip xs addrs, [Apply body l])
          else
            failString -< printf "Applied the function %s with %d arguments to %d arguments" (show e) (length xs) (length args)
      _ -> failString -< printf "Expected a function, but got %s" (show e)
    {-# SCC applyClosure #-}

{-# INLINEABLE eval #-}
{-# SCC eval #-}

run :: (ArrowChoice c,
        ArrowEnv Text addr c,
        ArrowStore addr v c,
        ArrowFail e c,
        IsString e,
        IsVal v c,
        Env.Join addr c,
        Fail.Join addr c,
        ArrowAlloc addr c,
        ArrowGarbageCollection v addr c,
        Eq addr, 
        Hashable addr)
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
    extendAddrs (proc (e,addr,rest) -> do
        val <- run' -< [e]
        write -< (addr,val)
        run' -< rest
      ) -< ([(x,addr)], (e,addr,rest))
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
  IsList_ v c,
  Env.Join v c,
  Env.Join addr c,
  Cls.Join v v c,
  Store.Join v c,
  Fail.Join v c,
  Fail.Join addr c,
  Join v c,
  ArrowAlloc addr c,
  Show addr,
  ArrowGarbageCollection v addr c,
  Eq addr,
  Hashable addr
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

  op1_ :: c (Op1, v) v
  op2_ :: c (Op2, v, v) v
  opvar_ :: c (OpVar, [v]) v

class (Arrow c) => IsList_ v c | c -> v where
  nil_ :: c Label v
  cons_ :: c ((v, Label), (v, Label)) v
  op1list_ :: c (Op1List,v) v 

extendAddrs :: (
    ArrowChoice c, 
    ArrowGarbageCollection v addr c,
    ArrowEnv Text addr c,
    Eq addr,
    Hashable addr) 
  => c x v -> c ([(Text,addr)],x) v 
extendAddrs f = proc (bnds,x) -> do 
  GC.addLocalGCRoots (Env.extend' f) -< (fromList $ map snd bnds, (bnds, x))

-- should this return a value or nothing ? 
collectAddrs :: (
    ArrowChoice c, 
    ArrowGarbageCollection v addr c,
    ArrowStore addr v c,
    Eq addr,
    Hashable addr)
  => c v v 
collectAddrs = proc retVal -> do 
  retAddrs <- GC.getAddrVal -< retVal 
  rootAddrs <- GC.getGCRoots -< () 
  store <- Store.store -< () 
  reachables <- GC.reachables -< (union retAddrs rootAddrs,store) 
  keys <- Store.keys -< () -- unnecessary if we already have store 
  removeables <- GC.collectables -< (keys,reachables) 
  map Store.remove -< toList removeables
  returnA -< retVal 