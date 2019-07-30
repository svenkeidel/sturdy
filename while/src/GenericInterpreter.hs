{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- | Generic interpreter for the While-Language.
module GenericInterpreter where

import           Prelude hiding (lookup, and, or, not, div, read)

import           Data.Label

import           Control.Arrow
import           Control.Arrow.Fail(ArrowFail)
import qualified Control.Arrow.Fail as Fail
import           Control.Arrow.Fix
import           Control.Arrow.Environment(ArrowEnv)
import qualified Control.Arrow.Environment as Env
import           Control.Arrow.Except(ArrowExcept)
import qualified Control.Arrow.Except as Except
import           Control.Arrow.Random
import           Control.Arrow.Store(ArrowStore,read',write)
import qualified Control.Arrow.Store as Store
import           Control.Arrow.Utils

import           Data.Text (Text)
import           Data.String

import           Syntax
import           GHC.Exts

type Prog = [Statement]

-- | Generic interpreter for expressions of the While-language. It is
-- an arrow computation @c Expr v@ that consumes expressions and
-- produces values. The interpreter is parameterized by the type of
-- values @v@, addresses @addr@, environment @env@ and arrow type
-- @c@. It uses the @IsVal@ interface to combine values and uses the
-- @ArrowEnv@ and @ArrowStore@ interface to access the environment.
eval :: (Show addr, ArrowChoice c, ArrowRand v c,
         ArrowEnv Text addr c, ArrowStore addr v c,
         ArrowExcept exc c, ArrowFail e c,
         IsVal v c, IsException exc v c, IsString e,
         Except.Join v c, Env.Join v c, Store.Join v c,
         Fail.Join v c
        )
     => c Expr v
eval = proc e -> case e of
  Var x _ -> Env.lookup'' read' -< x
  BoolLit b _ -> boolLit -< b
  And e1 e2 _ -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    and -< (v1,v2)
  Or e1 e2 _ -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    or -< (v1,v2)
  Not e1 _ -> do
    v1 <- eval -< e1
    not -< (v1)
  NumLit n _ -> numLit -< n
  RandomNum _ -> random -< ()
  Add e1 e2 _ -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    add -< (v1,v2)
  Sub e1 e2 _ -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    sub -< (v1,v2)
  Mul e1 e2 _ -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    mul -< (v1,v2)
  Div e1 e2 _ -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    div -< (v1,v2)
  Eq e1 e2 _ -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    eq -< (v1,v2)
  Lt e1 e2 _ -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    lt -< (v1,v2)
  Throw ex e1 _ -> do
    v <- eval -< e1
    Except.throw <<< namedException -< (ex,v)

-- | Generic interpreter for statements of the While-language. It is
-- an arrow computation @c [Statement] ()@ that consumes statements
-- and does not produces a result. The interpreter is parameterized by
-- the type of values @v@, addresses @addr@, environment @env@ and
-- arrow type @c@.
run :: (Show addr, ArrowChoice c, ArrowFix [Statement] () c,
        ArrowEnv Text addr c, ArrowStore addr v c,
        ArrowAlloc addr c, ArrowFail err c,
        ArrowExcept exc c, ArrowRand v c,
        IsString err, IsVal v c, IsException exc v c,
        Env.Join v c, Env.Join addr c,
        Store.Join v c, Except.Join v c, Except.Join () c,
        Fail.Join v c, JoinVal () c, JoinExc () c
       )
    => c [Statement] ()
run = fix $ \run' -> proc stmts -> case stmts of
  Assign x e l:ss -> do
    v <- eval -< e
    addr <- Env.lookup (proc (addr,_) -> returnA -< addr)
                       (proc (x,v,l) -> alloc -< (x,v,l))
              -< (x,(x,v,l))
    write -< (addr,v)
    Env.extend run' -< (x,addr,ss)
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
    Except.catch
      (proc (body,_) -> run' -< [body])
      (proc ((_,(ex,x,handler,l)),e) ->
        matchException
          (proc (v,(x,handler,l)) -> do
            addr <- Env.lookup pi1 alloc -< (x,(x,v,l))
            write -< (addr,v)
            Env.extend run' -< (x, addr, [handler]))
          (proc _ -> returnA -< ())
          -< (ex,e,(x,handler,l)))
      -< (body,(ex,x,handler,l))
    run' -< ss
  Finally body fin _:ss -> do
    Except.finally (proc (body,_) -> run' -< [body])
            (proc (_,fin)  -> run' -< [fin])
      -< (body,fin)
    run' -< ss
  [] ->
    returnA -< ()

-- | Interface for value operations.
class Arrow c => IsVal v c | c -> v where
  -- | In case of the abstract interpreter allows to join the result
  -- of an @if@ statement.
  type family JoinVal x (c :: * -> * -> *) :: Constraint

  boolLit :: c Bool v
  and :: c (v,v) v
  or :: c (v,v) v
  not :: c v v
  numLit :: c Int v
  add :: c (v,v) v
  sub :: c (v,v) v
  mul :: c (v,v) v
  div :: c (v,v) v
  eq :: c (v,v) v
  lt :: c (v,v) v
  if_ :: JoinVal z c => c x z -> c y z -> c (v, (x, y)) z

class ArrowAlloc addr c where
  alloc :: c (Text,v,Label) addr

class IsException exc v c | c -> v where
  type family JoinExc y (c :: * -> * -> *) :: Constraint
  namedException :: c (Text,v) exc
  matchException :: JoinExc y c => c (v,x) y -> c x y -> c (Text,exc,x) y
