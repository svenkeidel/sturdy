{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module SturdyStyle.GenericInterpreter where

import           Prelude hiding (lookup,and,fail)

import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Environment
import qualified Control.Arrow.Environment as Env
import           Control.Arrow.Store
import qualified Control.Arrow.Store as Store
import           Control.Arrow.Fail
import           Control.Arrow.Alloc

import           Data.Label

import           Syntax
import           GHC.Exts

-- | This interface abstracts over the values of the language.
-- The concrete and abstract interpreter implement this interface with
-- different types.
class Arrow c => IsValue v c | c -> v where
  numLit :: c Int v
  boolLit :: c Bool v
  add :: c (v,v) v
  and :: c (v,v) v
  lt :: c (v,v) v
  if_ :: c [Statement] () -> c [Statement] () -> c (v,[Statement],[Statement]) ()


eval :: ( Show addr, IsString e, IsValue v c, ArrowChoice c,

          -- The generic interpreter uses other language-independent
          -- interfaces for environments, stores, failure and
          -- fixpoints from the Sturdy standard library.
          ArrowEnv String addr env c, ArrowStore addr v c, ArrowFail e c,

          -- These @Join@ constraints are needed to allow the abstract
          -- interpreter to join two arrow computations.
          Env.Join c ((addr, String),String) v, Store.Join c ((v, addr),addr) v
        ) => c Expr v
eval = proc e -> case e of
  Var x _ -> lookup'' read' -< x
  NumLit n _ -> numLit -< n
  Add e1 e2 _ -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    add -< (v1,v2)
  BoolLit b _ -> boolLit -< b
  And e1 e2 _ -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    and -< (v1,v2)
  Lt e1 e2 _ -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    lt -< (v1,v2)

run :: (Show addr, IsString e, IsValue v c, ArrowChoice c,
        ArrowEnv String addr env c, ArrowStore addr v c, ArrowAlloc (String,v,Label) addr c,
        ArrowFail e c, ArrowFix [Statement] () c,
        Env.Join c ((addr, String),String) v, Env.Join c ((addr, (String,v,Label)), (String,v,Label)) addr,
        Store.Join c ((v, addr),addr) v) => c [Statement] ()
run = fix $ \run' -> proc stmts -> case stmts of
  (Assign x e l : rest) -> do
    v <- eval -< e
    addr <- lookup (proc (a,_) -> returnA -< a) alloc -< (x,(x,v,l))
    write -< (addr,v)
    extendEnv' run' -< (x,addr,rest)
  (If cond ifBranch elseBranch _ : rest) -> do
    v <- eval -< cond
    if_ run' run' -< (v,ifBranch,elseBranch)
    run' -< rest
  (While cond body l : rest) ->
    run' -< (If cond (body ++ [While cond body l]) [] l : rest)
  [] ->
    returnA -< ()

