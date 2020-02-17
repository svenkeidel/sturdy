{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module GenericInterpreter where

import           Prelude hiding (succ, pred, fail, map)
import           Syntax (Literal(..), Expr(..), Op1_(..), Op2_(..), OpVar_(..),list)

import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Environment(ArrowEnv)
import           Control.Arrow.LetRec(ArrowLetRec)
import qualified Control.Arrow.LetRec as LetRec
import           Control.Arrow.Fix.Context


-- import           Control.Arrow.LetRec (ArrowLetRec_)
-- import qualified Control.Arrow.LetRec as LetRec
import qualified Control.Arrow.Environment as Env
import           Control.Arrow.Closure (ArrowClosure)
import qualified Control.Arrow.Closure as Cls
import           Control.Arrow.Store (ArrowStore,write,read')
import qualified Control.Arrow.Store as Store 
import           Control.Arrow.Utils


import           Data.Text (Text,pack)
import           Data.Label (fresh)
import           Text.Printf
import           Data.List.Split

import           GHC.Exts (IsString(..),Constraint)

-- | Shared interpreter for Scheme.
eval :: (ArrowChoice c,
         ArrowFix (c [Expr] v),
         ArrowEnv Text addr c,
         ArrowStore addr v c,
         ArrowFail e c,
         IsString e,
         ArrowClosure Expr v c,
         ArrowLetRec Text v c,
         IsNum v c,
         Env.Join v c,
         ArrowList addr v c,
         Cls.Join v v c,
         Store.Join v c,
         Join v c,
         Show addr,
         ArrowAlloc addr c, 
         Env.Join addr c)
     => c [Expr] v -> c Expr v
eval run' = proc e0 -> case e0 of
  -- inner representation and evaluation
  Lit x _ -> lit -< x
  List [] l -> emptyList -< ()
  List xs l -> if length xs == 1 
    then evalList -< ([head xs], [])
    else evalList -< ([head xs], [List (tail xs) l])
  Cons x1 x2 l -> evalList -< ([x1],[x2])
  Begin es _ -> do
    run' -< es
  App e1 e2 _ -> do
    fun <- run' -< [e1]
    args <- map run' -< chunksOf 1 e2
    applyClosure' -< (fun, args)
  Apply es _ -> run' -< es
  -- Scheme expression
  Var x _ -> Env.lookup'' read' -< x
  Lam xs es l -> Cls.closure -< Lam xs es l 
  Let bnds body _ -> do -- iterative evaluation of bindings really necessary? 
    vs <- evalBindings -< bnds
    Env.extend' run' -< (vs,body)
  LetRec bnds body _ -> do
    vs <- evalBindings' -< bnds
    addrs <- map alloc -< [Left var | (var,_,val) <- vs]
    Env.extend' (LetRec.letRec run') -< ([(var,addr) | ((var,_,_),addr) <- zip vs addrs], ([(var,val) | (var,_,val) <- vs], body))
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
  where
    -- Helper function used to apply closure or a suspended fixpoint computation to its argument.
    applyClosure' = Cls.apply $ proc (e, args) -> case e of  -- args = [(argVal, argLabel)]
      Lam xs body l -> do
        if length xs == length args
          then do
            addrs <- map alloc -< map (\x -> Left x) xs
            map write -< zip addrs args
            Env.extend' run' -< (zip xs addrs, [Apply body l])
          else fail -< fromString $ "Applied a function to too many or too few arguments, params: "
      _ -> fail -< fromString $ "found unexpected epxression in closure: "  ++ show e

    evalBindings = proc bnds -> case bnds of
      [] -> returnA -< []
      (var,expr) : bnds' -> do
        val <- run' -< [expr]
        addr <- alloc -< Left var
        write -< (addr,val)
        vs <- Env.extend (evalBindings) -< (var, addr, bnds')
        returnA -< (var,addr) : vs

    evalBindings' = proc bnds -> do
     case bnds of
      [] -> returnA -< []
      (var,expr) : bnds' -> do
        val <- run' -< [expr]
        addr <- alloc -< Left var
        write -< (addr,val)
        --only adds closure to its own env so it can call itself recursively
        vs <- Env.extend (LetRec.letRec (evalBindings')) -< (var,addr,([(var,val)],bnds')) 
        returnA -< (var,addr,val) : vs

    evalList = proc (e1,e2) -> do 
      v1 <- run' -< e1
      a1 <- alloc -< Right $ head e1
      write -< (a1, v1)
      if (e2 == [])
        then do 
          v2 <- emptyList -< ()
          a2 <- alloc -< Left $ pack "Top" 
          write -< (a2, v2)
          list_ -< (a1,a2) 
        else do 
          v2 <- run' -< e2
          a2 <- alloc -< Right $ head e2
          write -< (a2, v2)
          list_ -< (a1,a2) 

run_ :: (ArrowChoice c,
         ArrowFix (c [Expr] v),
         ArrowEnv Text addr c,
         ArrowStore addr v c,
         ArrowLetRec Text v c,
         ArrowFail e c,
         IsString e,
         ArrowClosure Expr v c,
         IsNum v c,
         Env.Join v c,
         Env.Join addr c,
         Cls.Join v v c,
         Store.Join v c,
         Join v c,
         ArrowList addr v c,
         ArrowAlloc addr c,
         Show addr)
    => c [Expr] v
run_ = fix $ \run' -> proc es -> case es of
  [] ->
    fail -< fromString $ "Empty program"
  Set x e l:rest -> do
    v <- run' -< [e]
    addr <- Env.lookup (proc (addr, _) -> returnA -< addr)
               (proc var -> fail -< fromString $ printf "(set): Variable %s not bound when setting" (show var))
                 -< (x, x)
    write -< (addr,v)
    if rest == []
      then run' -< [Lit (String "#<void>") l]
      else run' -< rest
  Define x e l: rest -> do
    -- Not used except by test cases, which explicitly use define expression 
    -- TODO: Check whether pre_val == Undefined if so continue else => Error "Var has already been defined"
    cls <- run' -< [e]
    addr <- alloc -< Left x
    write -< (addr,cls)
    if rest == []
      then Env.extend (LetRec.letRec run') -< (x,addr,([(x,cls)], [Lit (String "#<void>") l]))
      else Env.extend (LetRec.letRec run') -< (x,addr,([(x,cls)],rest))
  e:[] ->
    eval run' -< e
  e:rest -> do
    eval run' -< e
    run' -< rest 


class ArrowAlloc addr c where
  alloc :: (ArrowEnv Text addr c, ArrowStore addr v c) => c (Either Text Expr) addr

class ArrowList addr v c where
  list_ :: c (addr,addr) v 
  cons_ :: c (addr,addr) v

-- | Interface for numeric operations
class Arrow c => IsNum v c | c -> v where
  type family Join y (c :: * -> * -> *) :: Constraint
  lit :: c Literal v
  if_ :: Join z c => c x z -> c y z -> c (v, (x, y)) z
  emptyList :: c () v

  op1_ :: c (Op1_, v) v
  op2_ :: c (Op2_, v, v) v
  opvar_ :: c (OpVar_, [v]) v
  