{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- | To show that an abstract interpreter produces correct results, we
-- have to prove that it soundly approximates the concrete interpreter.
-- However, with the previous styles, it is not explicit which parts
-- of the abstract interpreter we should relate to the concrete
-- interpreter. In contrast, this file contains a /generic interpreter/,
-- which makes it explicit which parts of the abstract and concrete
-- interpreter are related. It is parameterized by an interface, which
-- abstracts over the effects and values of the language. We implement
-- this interface with different types to instantiate the generic
-- interpreter for the concrete and abstract language semantics.
-- types.
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

import           Syntax (Expr(..),Statement(..))
import           GHC.Exts

-- | This interface abstracts over the values of the language.
class Arrow c => IsValue val c | c -> val where
  numLit :: c Int val
  boolLit :: c Bool val
  add :: c (val,val) val
  and :: c (val,val) val
  lt :: c (val,val) val
  if_ :: c x () -> c y () -> c (val,x,y) ()


-- | The generic interpreter uses other language-independent
-- interfaces for environments, stores, failure and fixpoints from the
-- Sturdy standard library. These `Join` constraints are needed to
-- allow the abstract interpreter to join two arrow computations.
eval :: ( Show addr, IsString err, IsValue val c, ArrowChoice c,
          ArrowEnv String addr env c, ArrowStore addr val c, ArrowFail err c,
          Env.Join c val, Store.Join c val
        ) => c Expr val
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

run :: (Show addr, Show env, Show val, IsString e, IsValue val c, ArrowChoice c,
        ArrowEnv String addr env c, ArrowStore addr val c, ArrowAlloc (String,val,Label) addr c,
        ArrowFail e c, ArrowFix [Statement] () c,
        Env.Join c val, Env.Join c addr, Store.Join c ((val, addr),addr) val)
    => c [Statement] ()
run = fix $ \run' -> proc stmts -> case stmts of
  (Assign x e l : rest) -> do
    v <- eval -< e
    addr <- lookup (proc (a,_) -> returnA -< a)
                   (proc l -> alloc -< l) -< (x,(x,v,l))
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

