  {-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module GenericInterpreter where

import           Prelude hiding (succ, pred, fail, map)
import           Syntax (Literal(..), Expr(..), Op1_(..), Op2_(..), OpVar_(..))

import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Environment(ArrowEnv,ArrowLetRec)
import qualified Control.Arrow.Environment as Env
import           Control.Arrow.Closure (ArrowClosure)
import qualified Control.Arrow.Closure as Cls
import           Control.Arrow.Store (ArrowStore,write)
import qualified Control.Arrow.Store as Store
import           Control.Arrow.Utils


import           Data.Text (Text)
import           Text.Printf
import           Data.List.Split

import           GHC.Exts (IsString(..),Constraint)

-- | Shared interpreter for Scheme.
eval :: (ArrowChoice c,
         ArrowFix (c [Expr] v),
         ArrowEnv Text v c,
         ArrowStore Text v c,
         ArrowFail e c,
         IsString e,
         ArrowClosure Expr v c,
         ArrowLetRec Text v c,
         IsNum v c,
         Env.Join v c,
         Cls.Join v v c,
         Store.Join v c,
         Join v c)
     => c [Expr] v -> c Expr v
eval run' = proc e0 -> case e0 of
  -- inner representation and evaluation
  Lit x _ -> lit -< x
  Begin es _ -> do
    run' -< es
  App e1 e2 _ -> do
    fun <- run' -< [e1]
    args <- map run' -< chunksOf 1 e2
    applyClosure' -< (fun, args)
  Apply es _ -> run' -< es
  -- Scheme expression
  Var x _ -> do
    Env.lookup' -< x
  Lam xs es l -> Cls.closure -< Lam xs es l
  Let bnds body _ -> do
    vs <- evalBindings -< bnds
    Env.extend' run' -< (vs,body)
  LetRec bnds body _ -> do
    vs <- evalBindings' -< bnds
    Env.letRec run' -< (vs, body)
    -- args <- map run' -< chunksOf 1 [ e | (_,e) <- bnds ]
    -- Env.letRec run' -< ([ (v,cl) | ((v,_),cl) <- zip bnds args ], body)
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
          then Env.extend' run' -< (zip xs args, [Apply body l])
          -- then Env.extend' run' -< (zip xs args, [Apply body l])
          -- then do extendMultiple -< zip xs args
          --         run' -< [Apply body l]
          else fail -< fromString $ "Applied a function to too many or too few arguments, params: "
      _ -> fail -< fromString $ "found unexpected epxression in closure: "  ++ show e

    -- extendMultiple = proc bnds -> case bnds of 
    --   [] -> returnA -< []
    --   (var, val) : bnds' -> do
    --     Env.extend extendMultiple -< (var, val, bnds')

    evalBindings = proc bnds -> case bnds of
      [] -> returnA -< []
      (var,expr) : bnds' -> do
        v <- run' -< [expr]
        vs <- Env.extend evalBindings -< (var, v, bnds')
        returnA -< (var,v) : vs

    evalBindings' = proc bnds -> case bnds of
      [] -> returnA -< []
      (var,expr) : bnds' -> do
        v <- run' -< [expr]
        vs <- Env.letRec evalBindings' -< ([(var, v)], bnds')
        returnA -< (var,v) : vs


run_ :: (ArrowChoice c,
        ArrowFix (c [Expr] v),
        ArrowEnv Text v c,
        ArrowStore Text v c,
        ArrowFail e c,
        ArrowClosure Expr v c,
        ArrowLetRec Text v c,
        IsString e,
        IsNum v c,
        Env.Join v c,
        Cls.Join v v c,
        Store.Join v c,
        Join v c)
    => c [Expr] v
run_ = fix $ \run' -> proc es -> case es of
  [] ->
    fail -< fromString $ "Empty program"
  Set x e l:rest -> do
    v <- run' -< [e]
    Env.lookup (proc (val, _) -> returnA -< val)
               (proc var -> fail -< fromString $ printf "(set): Variable %s not bound when setting" (show var))
                 -< (x, x)
    write -< (x,v)
    if rest == []
      then run' -< [Lit (String "#<void>") l]
      else run' -< rest
  Define x e l: rest -> do
    -- TODO: Check whether pre_val == Undefined if so continue else => Error "Var has already been defined"
    cls <- run' -< [e]
    -- Env.lookup (proc (_,(var,_,_)) -> fail -< fromString $ printf "Variable %s already exists" (show var))
    --                    (proc (x,v,l) -> _ -< (x,v,l))
    --           -< (x, (x,cls,l))
    if rest == []
      then Env.letRec run' -< ([(x,cls)],[Lit (String "#<void>") l])
      else Env.letRec run' -< ([(x,cls)],rest)
      -- then Env.extend run' -< (x,cls,[Lit (String "#<void>") l])
      -- else Env.extend run' -< (x,cls,rest)
      -- else Env.extend run' -< (x,cls,rest)

  e:[] ->
    eval run' -< e
  e:rest -> do
    eval run' -< e
    run' -< rest


-- | Interface for numeric operations
class Arrow c => IsNum v c | c -> v where
  type family Join y (c :: * -> * -> *) :: Constraint
  lit :: c Literal v
  if_ :: Join z c => c x z -> c y z -> c (v, (x, y)) z

  op1_ :: c (Op1_, v) v
  op2_ :: c (Op2_, v, v) v
  opvar_ :: c (OpVar_, [v]) v
  