{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- | Generic interpreter for the While-Language.
module Exceptions.GenericInterpreter where

import           Prelude hiding (lookup, and, or, not, div, read)

import           Data.Label

import           GenericInterpreter(IsVal(..))

import           Control.Arrow
import           Control.Arrow.Alloc
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Environment(ArrowEnv,lookup,lookup'',extendEnv')
import qualified Control.Arrow.Environment as Env
import           Control.Arrow.Random
import           Control.Arrow.Store(ArrowStore,read',write)
import           Control.Arrow.Except(ArrowExcept,throw,catch,finally)
import qualified Control.Arrow.Except as Except
import qualified Control.Arrow.Store as Store
import           Control.Arrow.Utils

import           Data.Text (Text)
import           Data.String

import           Exceptions.Syntax hiding (finally)
import           GHC.Exts

type Prog = [Statement]

-- | Generic interpreter for expressions of the While-language. It is
-- an arrow computation @c Expr v@ that consumes expressions and
-- produces values. The interpreter is parameterized by the type of
-- values @v@, addresses @addr@, environment @env@ and arrow type
-- @c@. It uses the @IsVal@ interface to combine values and uses the
-- @ArrowEnv@ and @ArrowStore@ interface to access the environment.
eval :: (Show addr, ArrowChoice c, ArrowRand v c,
         ArrowEnv Text addr env c, ArrowStore addr v c,
         ArrowFail err c, IsString err, ArrowExcept exc c,
         IsVal v c, IsException exc v c,
         Env.Join c ((addr, Text),Text) v,
         Store.Join c ((v, addr),addr) v
        )
     => c Expr v
eval = proc e -> case e of
  Var x _ -> lookup'' read' -< x
  BoolLit b l -> boolLit -< (b,l)
  And e1 e2 l -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    and -< (v1,v2,l)
  Or e1 e2 l -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    or -< (v1,v2,l)
  Not e1 l -> do
    v1 <- eval -< e1
    not -< (v1,l)
  NumLit n l -> numLit -< (n,l)
  RandomNum _ -> random -< ()
  Add e1 e2 l -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    add -< (v1,v2,l)
  Sub e1 e2 l -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    sub -< (v1,v2,l)
  Mul e1 e2 l -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    mul -< (v1,v2,l)
  Div e1 e2 l -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    div -< (v1,v2,l)
  Eq e1 e2 l -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    eq -< (v1,v2,l)
  Lt e1 e2 l -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    lt -< (v1,v2,l)
  Throw ex e1 _ -> do
    v <- eval -< e1
    throw <<< namedException -< (ex,v)

-- | Generic interpreter for statements of the While-language. It is
-- an arrow computation @c [Statement] ()@ that consumes statements
-- and does not produces a result. The interpreter is parameterized by
-- the type of values @v@, addresses @addr@, environment @env@ and
-- arrow type @c@.
run :: (Show addr, ArrowChoice c, ArrowFix [Statement] () c,
        ArrowEnv Text addr env c, ArrowStore addr v c,
        ArrowAlloc (Text,v,Label) addr c, ArrowFail err c,
        ArrowExcept exc c, ArrowRand v c, IsString err,
        IsVal v c, IsException exc v c,
        Env.Join c ((addr, Text),Text) v, Env.Join c ((addr, (Text,v,Label)), (Text,v,Label)) addr,
        Store.Join c ((v, addr),addr) v,
        Except.Join c ((), ((Statement, (Text, Text, Statement, Label)), exc)) (),
        Except.Join c (((Statement, Statement), ()), ((Statement, Statement), exc)) (),
        JoinVal c ([Statement],[Statement]) (),
        JoinExc c ((v, (Text, Statement, Label)), (Text, Statement, Label)) ()
       )
    => c [Statement] ()
run = fix $ \run' -> proc stmts -> case stmts of
  Assign x e l:ss -> do
    v <- eval -< e
    addr <- lookup (proc (addr,_) -> returnA -< addr)
                   (proc (x,v,l) -> alloc -< (x,v,l))
                   -< (x,(x,v,l))
    write -< (addr,v)
    extendEnv' run' -< (x,addr,ss)
  If cond s1 s2 _:ss -> do
    b <- eval -< cond
    if_ run' run' -< (b,([s1],[s2]))
    run' -< ss
  While cond body l:ss ->
    run' -< If cond (Begin [body,While cond body l] l) (Begin [] l) l : ss
  Begin ss _:ss' -> do
    run' -< ss
    run' -< ss'
  TryCatch body ex x handler l:ss -> do
    catch
      (proc (body,_) -> run' -< [body])
      (proc ((_,(ex,x,handler,l)),e) ->
        matchException
          (proc (v,(x,handler,l)) -> do
            addr <- lookup pi1 alloc -< (x,(x,v,l))
            write -< (addr,v)
            extendEnv' run' -< (x, addr, [handler]))
          (proc _ -> returnA -< ())
          -< (ex,e,(x,handler,l)))
      -< (body,(ex,x,handler,l))
    run' -< ss
  Finally body fin _:ss -> do
    finally (proc (body,_) -> run' -< [body])
            (proc (_,fin)  -> run' -< [fin])
      -< (body,fin)
    run' -< ss
  [] ->
    returnA -< ()

class IsException exc v c | c -> v where
  type family JoinExc (c :: * -> * -> *) x y :: Constraint
  namedException :: c (Text,v) exc
  matchException :: JoinExc c ((v,x),x) y => c (v,x) y -> c x y -> c (Text,exc,x) y
