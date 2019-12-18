{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module GenericInterpreter where

import Prelude hiding (rem,mod,div,id,or,and,fail,return,map)

import Data.Text (Text)
import Data.Vector (Vector,(!?))
import qualified Data.Vector as Vec
import Data.Int
import Data.IntMap (IntMap)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import Control.Category

import Control.Arrow
import Control.Arrow.Environment as Env
import Control.Arrow.Frame as Frame
import Control.Arrow.Except as Except
import Control.Arrow.Fail
import Control.Arrow.Fix (ArrowFix(..))
import Control.Arrow.Utils (map)

import GHC.Exts

import Syntax

data Frame val =
  Frame
  { this            :: val
  , params          :: Vector val
  , locals          :: HashMap Text val
  , stmts           :: Vector Statement
  , handlers        :: HashMap ClassId CatchClause
  , caughtException :: Maybe val
  }

type PC = Int

type StmtInterpreter c = c PC ()

type ArrowInterp val e c =
  ( IsString e, IsVal val c
  , ArrowChoice c, ArrowReturn val c
  , ArrowFrame (Frame val) c, ArrowEnv Variable val c
  , ArrowExcept val c, ArrowFail e c
  , ArrowFix (StmtInterpreter c)
  , JoinVal () c, JoinVal val c, Env.Join val c, Except.Join () c
  )

eval :: ArrowInterp val e c => StmtInterpreter c -> c Expr val
eval run' = proc expr -> case expr of
  New typ -> new -< typ
  NewArray typ e -> do
    len <- evalImmediate -< e
    newArray -< (typ,[len])
  NewMultiArray typ es -> do
    lens <- map evalImmediate -< es
    newArray -< (typ,lens)
  Cast typ e -> do
    val <- evalImmediate -< e
    cast -< (typ,val)
  InstanceOf e typ -> do
    val <- evalImmediate -< e
    instanceOf -< (val,typ)
  InvokeExpr method ->
    evalInvoke run' -< method
  Ref ref -> lookup' -< ReferenceVar ref
  Binop e1 op e2 -> do
    v1 <- evalImmediate -< e1
    v2 <- evalImmediate -< e2
    case op of
      And -> and -< (v1,v2)
      Or -> or -< (v1,v2)
      Xor -> xor -< (v1,v2)
      Rem -> rem -< (v1,v2)
      Mod -> mod -< (v1,v2)
      Cmp -> cmp -< (v1,v2)
      Cmpl -> cmpl -< (v1,v2)
      Cmpg -> cmpg -< (v1,v2)
      Shl -> shl -< (v1,v2)
      Shr -> shr -< (v1,v2)
      Ushr -> ushr -< (v1,v2)
      Plus -> plus -< (v1,v2)
      Minus -> minus -< (v1,v2)
      Mult -> mult -< (v1,v2)
      Div -> div -< (v1,v2)
  Unop op e -> do
    v <- evalImmediate -< e
    case op of
      Lengthof -> lengthOf -< v
      Neg -> neg -< v
  Immediate e -> evalImmediate -< e
  MethodHandle {} -> fail -< "Unsupported operation: MethodHandle"
{-# INLINE eval #-}

evalImmediate :: ArrowInterp val e c => c Immediate val
evalImmediate = proc i -> case i of
  Local name -> lookup' -< LocalVar name
  DoubleConstant f -> doubleConstant -< f
  FloatConstant f -> floatConstant -< f
  IntConstant n -> intConstant -< n
  LongConstant f -> longConstant -< f
  NullConstant -> nullConstant -< ()
  StringConstant s -> stringConstant -< s
  ClassConstant c -> classConstant -< c
{-# INLINE evalImmediate #-}

evalInvoke :: ArrowInterp val e c => StmtInterpreter c -> c Invoke val
evalInvoke run' = proc e -> case e of
  InvokeVirtual obj klass methodSig args -> do
    receiver <- lookup' -< obj
    invoke -< (receiver,klass,methodSig,args)
  InvokeSpecial obj klass methodSig args -> do
    receiver <- lookup' -< obj
    invoke -< (receiver,klass,methodSig,args)
  InvokeInterface obj klass methodSig args -> do
    receiver <- lookup' -< obj
    invoke -< (receiver,klass,methodSig,args)
  InvokeStatic klass methodSig args -> do
    receiver <- lookup' -< StaticInstance klass
    invoke -< (receiver,klass,methodSig,args)
  -- InvokeDynamic {} -> fail -< "We currently do not support dynamic method lookup"
  where
    invoke = proc (receiver,klass,methodSig,args) -> do
      argVals <- map evalImmediate -< args
      lookupMethod (proc (receiver,body,argVals) -> do
          let frame = Frame { this = receiver
                            , params = Vec.fromList argVals
                            , locals = Map.empty
                            , stmts = statements body
                            , handlers = catchClauses body
                            , caughtException = Nothing
                            }
          newFrame (handleReturn run') -< (frame,0)
        ) -< (receiver,klass,methodSig,argVals)
    {-# INLINE invoke #-}
{-# INLINE evalInvoke #-}


run :: ArrowInterp val e c => StmtInterpreter c
run = fix $ \run' -> handleExceptions $ proc pc -> do
  let nextStmt = pc + 1
  frame <- askFrame -< ()
  case stmts frame !? pc of
    Nothing -> returnA -< ()
    Just stmt -> case stmt of
      Goto lab -> run' -< lab
      Label {} -> run' -< nextStmt
      If e lab -> do
        condition <- eval run' -< e
        if_ run' run' -< (condition,(lab,nextStmt))
      TableSwitch key offset cases def -> do
        val <- evalImmediate -< key
        tableswitch run' -< (val,offset,cases,def)
      LookupSwitch key cases def -> do
        val <- evalImmediate -< key
        lookupswitch run' -< (val,cases,def)
      Identity var ident _maybeTyp -> do
        val <- lookup' -< ident
        extend run' -< (LocalVar var,val,nextStmt)
      Assign var e -> do
        val <- eval run' -< e
        extend run' -< (var,val,nextStmt)
      InvokeStmt invoke -> do
        evalInvoke run' -< invoke
        run' -< nextStmt
      Return Nothing  -> returnA -< ()
      Return (Just e) -> return <<< evalImmediate -< e
      Throw e -> throw <<< evalImmediate -< e
      Nop -> run' -< nextStmt
      Breakpoint {} -> run' -< nextStmt

      -- Unsupported Operations
      Ret {}          -> fail -< "JVM subroutines are not supported"
      EnterMonitor {} -> fail -< "JVM monitor statements are not supported"
      ExitMonitor {}  -> fail -< "JVM monitor statements are not supported"
{-# INLINE run #-}

handleExceptions :: ArrowInterp val e c => StmtInterpreter c -> StmtInterpreter c
handleExceptions run' = catch run' $ proc (pc,exc) -> do
  frame <- askFrame -< ()
  matchException (proc (exc,handler) -> Env.extend run' -< (CaughtException,exc,withLabel handler)) -< (exc,pc,handlers frame)
{-# INLINE handleExceptions #-}

class ArrowReturn v c where
  return :: c v x
  handleReturn :: c x y -> c x v

-- | Interface for value operations.
class IsVal v c | c -> v where
  -- | In case of the abstract interpreter allows to join the result
  -- of an @if@ statement.
  type family JoinVal x (c :: * -> * -> *) :: Constraint
  if_ :: JoinVal z c => c x z -> c y z -> c (v, (x, y)) z
  tableswitch :: JoinVal y c => c x y -> c (v, Int, Vector Label, Label) y
  lookupswitch :: JoinVal y c => c x y -> c (v, IntMap Label, Label) y
  lookupMethod :: JoinVal y c => c (v,MethodBody,x) y -> c (v,ClassId,MethodSignature,x) y
  matchException :: JoinVal y c => c (v,CatchClause) y -> c (v,PC,HashMap ClassId CatchClause) y
  new :: c Type v
  newArray :: c (Type,[v]) v
  doubleConstant :: c Double v
  floatConstant :: c Float v
  intConstant :: c Int32 v
  longConstant :: c Int64 v
  nullConstant :: c () v
  stringConstant :: c Text v
  classConstant :: c Text v
  and :: c (v,v) v
  or :: c (v,v) v
  xor :: c (v,v) v
  rem :: c (v,v) v
  mod :: c (v,v) v
  cmp :: c (v,v) v
  cmpg :: c (v,v) v
  cmpl :: c (v,v) v
  shl :: c (v,v) v
  shr :: c (v,v) v
  ushr :: c (v,v) v
  plus :: c (v,v) v
  minus :: c (v,v) v
  mult :: c (v,v) v
  lengthOf :: c v v
  div :: c (v,v) v
  neg :: c v v
  cast :: c (Type,v) v
  instanceOf :: c (v,Type) v
