{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-type-signatures #-}
-- | k-CFA analysis for PCF where numbers are approximated by intervals.
module TypedAnalysis where

import           Prelude hiding (not,Bounded,fail,(.),exp,read)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Environment as Env
import           Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Chaotic(chaotic,iterateInner,iterateOuter)
import           Control.Arrow.Fix.Parallel
import qualified Control.Arrow.Fix.Context as Ctx
import           Control.Arrow.Trans
import           Control.Arrow.Closure (ArrowClosure,IsClosure(..))
import qualified Control.Arrow.Closure as Cls
import           Control.Arrow.Order
import           Control.Arrow.Store
import qualified Control.Arrow.Store as Store
import qualified Control.Arrow.Utils as ArrowUtils
import           Control.Arrow.Fix.Context
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.FiniteEnvStore
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Chaotic
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Stack
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable(CacheT,Monotone)
import           Control.Arrow.Transformer.Abstract.Terminating

import           Control.Monad.State hiding (lift,fail)

import           Data.Hashable
import           Data.Label
import           Data.Order
import           Data.Text (Text, unpack)
import           Data.List (intersect)
import           Data.Utils
import           Data.HashMap.Lazy (HashMap)
import qualified Data.Boolean as B
import qualified Data.HashMap.Lazy as Map
import           Data.HashSet(HashSet)

import qualified Data.Abstract.Boolean as B
import           Data.Abstract.Error (Error)
import qualified Data.Abstract.Widening as W
import           Data.Abstract.Terminating(Terminating)
import           Data.Abstract.Closure (Closure)
import           Data.Abstract.DiscretePowerset (Pow)
import           Data.Abstract.CallString(CallString)

import           GHC.Exts(IsString(..),fromList, toList)
import           GHC.Generics(Generic)

import           Syntax (Expr(..),Literal(..) ,Op1_(..),Op2_(..),OpVar_(..), apply)
import           GenericInterpreter as Generic

type Cls = Closure Expr (HashSet (HashMap Text Addr))
type Env = HashMap Text Addr
type Store = HashMap Addr Val
type Ctx = CallString Label

data Addr 
  = VarA (Text,Ctx)
  | CellA (Expr,Ctx)
  | TopA deriving (Eq,Generic)      

-- | Numeric values are approximated with bounded intervals, closure
-- values are approximated with a set of abstract closures.
type Val = Pow Primitives
data Primitives
  = IntVal 
  | FloatVal
  | BoolVal B.Bool 
  | ClosureVal Cls 
  | StringVal
  | QuoteVal
  | ListVal Addr Addr
  | EmptyList 
  | Top
  | Bottom
  deriving (Eq, Generic)
data ListT 
  = Nil 
  | Coons Text Text
  deriving (Eq, Generic)

-- Input and output type of the fixpoint.
type In = (Store,(([Expr],Label),Env))
type Out = (Store, Terminating (Error (Pow String) Val))
type Out' = (Monotone
              (Store, (([Expr], Label), Env))
              (Store, Terminating (Error (Pow String) Val)),
            (HashMap Addr Val, Terminating (Error (Pow String) Val)))
-- | Run the abstract interpreter for an interval analysis. The arguments are the
-- maximum interval bound, the depth @k@ of the longest call string,
-- an environment, and the input of the computation.
evalIntervalChaotic :: (?sensitivity :: Int) => [(Text,Addr)] -> [State Label Expr] -> Out'
evalIntervalChaotic env0 e = run (extend' (Generic.run_ ::
      Fix'
        (ValueT Val
          (ErrorT (Pow String)
            (TerminatingT
              (EnvStoreT Text Addr Val 
                (FixT _ _
                    (ChaoticT In
                      (StackT Stack In
                        (CacheT Monotone In Out
                          (ContextT Ctx  
                              (->)))))))))) [Expr] Val))
    W.finite
    iterationStrategy
    (W.finite, W.finite)
    (Map.empty,(Map.empty,(env0,e0)))
  where
    e0 = generate (sequence e)
    iterationStrategy =
      -- Fix.traceShow .
      Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of [App _ _ l] -> Just l; _ -> Nothing) .
      Fix.filter apply chaotic -- parallel -- iterateInner

evalIntervalChaoticInner :: (?sensitivity :: Int) => [(Text,Addr)] -> [State Label Expr] -> Out'
evalIntervalChaoticInner env0 e = run (extend' (Generic.run_ ::
      Fix'
        (ValueT Val
          (ErrorT (Pow String)
            (TerminatingT
              (EnvStoreT Text Addr Val 
                (FixT _ _
                    (ChaoticT In
                      (StackT Stack In
                        (CacheT Monotone In Out
                          (ContextT Ctx  
                              (->)))))))))) [Expr] Val))
    W.finite
    iterationStrategy
    (W.finite, W.finite)
    (Map.empty,(Map.empty,(env0,e0)))
  where
    e0 = generate (sequence e)
    iterationStrategy =
      -- Fix.traceShow .
      Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of [App _ _ l] -> Just l; _ -> Nothing) .
      Fix.filter apply iterateInner

evalIntervalChaoticOuter :: (?sensitivity :: Int) => [(Text,Addr)] -> [State Label Expr] -> Out'
evalIntervalChaoticOuter env0 e = run (extend' (Generic.run_ ::
      Fix'
        (ValueT Val
          (ErrorT (Pow String)
            (TerminatingT
              (EnvStoreT Text Addr Val 
                (FixT _ _
                    (ChaoticT In
                      (StackT Stack In
                        (CacheT Monotone In Out
                          (ContextT Ctx  
                              (->)))))))))) [Expr] Val))
    W.finite
    iterationStrategy
    (W.finite, W.finite)
    (Map.empty,(Map.empty,(env0,e0)))
  where
    e0 = generate (sequence e)
    iterationStrategy =
      -- Fix.traceShow .
      Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of [App _ _ l] -> Just l; _ -> Nothing) .
      Fix.filter apply iterateOuter

evalIntervalParallel :: (?sensitivity :: Int) => [(Text,Addr)] -> [State Label Expr] -> Out'
evalIntervalParallel env0 e = run (extend' (Generic.run_ ::
      Fix'
        (ValueT Val
          (ErrorT (Pow String)
            (TerminatingT
              (EnvStoreT Text Addr Val 
                (FixT _ _
                  (StackT Stack In
                    (CacheT Monotone In Out
                      (ContextT Ctx  
                          (->))))))))) [Expr] Val))
    W.finite
    iterationStrategy
    (W.finite, W.finite)
    (Map.empty,(Map.empty,(env0,e0)))
  where
    e0 = generate (sequence e)
    iterationStrategy =
      -- Fix.traceShow .
      Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of [App _ _ l] -> Just l; _ -> Nothing) .
      Fix.filter apply parallel      

instance (ArrowChoice c, ArrowContext Ctx c, ArrowFail e c, IsString e) 
    => ArrowAlloc Addr (ValueT Val c) where
  alloc = proc x -> case x of 
    Left "Top" -> returnA -< TopA
    Left var -> do 
      ctx <- Ctx.askContext @Ctx -< ()
      returnA -< VarA (var,ctx)
    Right e -> do 
      ctx <- Ctx.askContext @Ctx -< ()
      returnA -< CellA (e,ctx)

instance (ArrowStore Addr Val c, Store.Join Val c, ArrowChoice c, ArrowFail e c, IsString e) 
    => ArrowList Addr Val (ValueT Val c) where
  list_ = proc (a1,a2) -> returnA -< singleton $ ListVal a1 a2
  cons_ = proc (a1,a2) -> returnA -< singleton $ ListVal a1 a2

instance (IsString e, ArrowChoice c, ArrowFail e c, ArrowClosure Expr Cls c)
    => ArrowClosure Expr Val (ValueT Val c) where
  type Join y Val (ValueT Val c) = Cls.Join y Cls c
  closure = ValueT $ proc e -> do 
    cls <- Cls.closure -< e
    returnA -< singleton (ClosureVal cls)
  -- TODO: 
  -- fix, apply only works for 1 cls in set of vals 
  apply (ValueT f) = ValueT $ proc (v,x) -> do
    let clss = getCls $ toList v
    Cls.apply f -< head $ zip clss (repeat x)  
  -- END TODO
  {-# INLINE closure #-} 
  {-# INLINE apply #-}

instance (ArrowEnv Text Addr c, Store.Join Val c, Env.Join Addr c,Store.Join Addr c, IsString e, ArrowChoice c, ArrowFail e c, ArrowStore Addr Val c) 
    => IsNum Val (ValueT Val c) where
  type Join y (ValueT Val c) = ArrowComplete y (ValueT Val c)
  lit = proc x -> case x of
    Number _ -> returnA -< singleton IntVal
    Float _ -> returnA -< singleton FloatVal
    Ratio _ -> returnA -< singleton Bottom
    Bool True  -> returnA -< singleton $ BoolVal B.True 
    Bool False  -> returnA -< singleton $ BoolVal B.False
    Char _ -> returnA -< singleton StringVal
    String _ -> returnA -< singleton StringVal
    Quote _ -> returnA -< singleton QuoteVal
    _ -> returnA -< singleton Bottom
  if_ f g = proc (v,(x,y)) ->
    if isTrue v && isFalse v
      then  (f -< x) <⊔> (g -< y)
      else if isTrue v
        then f -< x
        else if isFalse v
          then g -< y 
          else fail -< "if: should not happen"
  emptyList = proc _ -> returnA -< singleton EmptyList
  op1_ = proc (op, x) -> case op of
    Number_ -> withVal (\val -> case val of 
      IntVal -> BoolVal B.True
      _ -> BoolVal B.False) -< x
    Integer_ -> withVal (\val -> case val of 
      IntVal -> BoolVal B.True 
      _ -> BoolVal B.False) -< x 
    Float_ -> withVal (\val -> case val of 
      FloatVal -> BoolVal B.True 
      _ -> BoolVal B.False) -< x 
    Ratio_ -> withVal (\val -> case val of 
      IntVal -> BoolVal B.Top
      FloatVal -> BoolVal B.Top
      _ -> BoolVal B.False) -< x 
    Zero -> withVal unArithmetics -< x
    Positive -> withVal unArithmetics -< x 
    Negative -> withVal unArithmetics -< x 
    Odd -> withVal unArithmetics -< x 
    Even -> withVal unArithmetics -< x 
    Abs -> withVal unArithmetics -< x
    Floor -> withVal unArithmetics -< x
    Ceiling -> withVal unArithmetics -< x 
    Log -> withVal (\val -> case val of 
      IntVal -> FloatVal
      FloatVal -> FloatVal 
      _ -> Bottom) -< x 
    Boolean -> withVal (\val -> case val of 
      BoolVal _ -> BoolVal B.True
      _ -> BoolVal B.False) -< x 
    Not -> withVal (\val -> case val of -- js wat talk
      BoolVal b -> BoolVal $ B.not b
      _ -> Bottom) -< x 
    Null -> withVal (\val -> case val of 
      EmptyList -> BoolVal B.True
      _ -> BoolVal B.False) -< x        
    ListS -> if containsList (toList x) 
      then if allList (toList x) 
        then returnA -< singleton $ BoolVal B.True 
        else returnA -< singleton $ BoolVal B.Top
      else returnA -< singleton $ BoolVal B.False
    Car -> do 
      let addrs = carHelp $ toList x 
      vals <- ArrowUtils.map read' -< addrs
      if vals == empty
        then returnA -< singleton EmptyList 
        else returnA -< foldl1 (⊔) vals
    Cdr -> do
      let addrs = cdrHelp $ toList x 
      vals <- ArrowUtils.map read' -< addrs
      if vals == empty
        then returnA -< singleton EmptyList 
        else returnA -< foldl1 (⊔) vals 
    Caar -> do
      v1 <- op1_ -< (Car, x)
      op1_ -< (Car, v1)
    Cadr -> do
      v1 <- op1_ -< (Cdr, x)
      op1_ -< (Car, v1)
    Cddr -> do
      v1 <- op1_ -< (Cdr, x)
      op1_ -< (Cdr, v1)
    Caddr -> do
      v2 <- op1_ -< (Cdr, x)
      op1_ -< (Car, v2)
    Error -> returnA -< singleton Bottom
  op2_ = proc (op, x, y) -> case op of
    Eqv -> eqHelp -< (x,y)
  --     /** (define (equal? a b)
  --       (or (eq? a b)
  --         (and (null? a) (null? b))
  --         (and (pair? a) (pair? b) (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
  --         (and (vector? a) (vector? b)
  --           (let ((n (vector-length a)))
  --             (and (= (vector-length b) n)
  --               (letrec ((loop (lambda (i)
  --                                (or (= i n)
  --                                  (and (equal? (vector-ref a i) (vector-ref b i))
  --                                    (loop (+ i 1)))))))
  --                 (loop 0)))))))
  --  */
    Equal -> do 
      eq <- op2_ -< (Eqv,x,y)
      t <- if__ returnA returnA -< (isTrue eq, (eq, singleton Bottom))
      f <- if__ nullH returnA -< (isFalse eq, ((eq,(x,y)), singleton Bottom))
      returnA -< t ⊔ f
      
      -- v <- if__ returnA nullH -< (eq, ((),(x,y))) 
      -- v' <- if__ returnA listH -< (null1 == true && null1 == null2,((),(x,y)))

      --   True -> returnA -< true
      --   Top -> 
      --   False ->
      --   then returnA -< eq
      --   else do 
      --     null1 <- op1_ -< (Null,x)
      --     null2 <- op1_ -< (Null,y)
      --     if null1 == true &&
      --        null2 == true
      --        then return -< true
      --        else do    
      --         list1 <- op1_ -< (ListS,x)
      --         list2 <- op1_ -< (ListS,y)
      --         if list1 == true && list2 == true 
      --           then do 
      --             car1 <- op1_ -< (Car, x)
      --             car2 <- op1_ -< (Car, y)
      --             eq1 <- op2_ -< (Equal,car1,car2)
      --             if eq1 == false 
      --               then returnA -< false
      --               else 
      --           else returnA -< singleton $ BoolVal top
      -- if eq == (singleton $ BoolVal B.True ) ||
      --   (null1 == (singleton $ BoolVal B.True) &&
      --    null2 == (singleton $ BoolVal B.True))
      --    then returnA -< singleton $ BoolVal B.True
      --    else do 
      --     car1 <- op1_ -< (Car, x)
      --     car2 <- op1_ -< (Car, y)
      --     cdr1 <- op1_ -< (Cdr, x)
      --     cdr2 <- op1_ -< (Cdr, y)
      --     eq1 <- op2_ -< (Equal,car1,car2)
      --     eq2 <- op2_ -< (Equal,cdr1,cdr2)
      --     if (list1 == (singleton $ BoolVal B.True) &&
      --         list2 == (singleton $ BoolVal B.True) &&
      --         eq1 == (singleton $ BoolVal B.True) &&
      --         eq2 == (singleton $ BoolVal B.True))
      --     then returnA -< singleton $ BoolVal B.True
      --     else if list1 == (singleton $ BoolVal B.False) ||
      --             list2 == (singleton $ BoolVal B.False) 
      --       then returnA -< eq
      --       else returnA -< singleton $ BoolVal B.Top
    Quotient -> with2Val -< (x,y) 
    Remainder -> with2Val -< (x,y)
    Modulo -> with2Val -< (x,y)
  opvar_ =  proc (op, xs) -> case op of
    EqualS -> withVarValNumBool -< xs
    SmallerS -> withVarValNumBool -< xs
    GreaterS -> withVarValNumBool -< xs 
    SmallerEqualS -> withVarValNumBool -< xs 
    GreaterEqualS -> withVarValNumBool -< xs  
    Max -> withVarValNumNum -< xs  
    Min -> withVarValNumNum -< xs  
    Add -> withVarValNumNum -< xs 
    Mul -> withVarValNumNum -< xs
    Sub -> withVarValNumNum -< xs
    Div -> withVarValNumNum' -< xs 
    Gcd -> withVarValNumNum -< xs 
    Lcm -> withVarValNumNum -< xs 
    -- and / or not needed as it is desugared to if statements 
    And -> withVarValBoolBool -< xs  
    Or -> withVarValBoolBool -< xs 


  {-# INLINE lit #-}
  {-# INLINE if_ #-}
  {-# INLINE op1_ #-}
  {-# INLINE op2_ #-}
  {-# INLINE opvar_ #-}

nullH :: (IsNum Val c, Arrow c, ArrowChoice c) => c (Val,(Val,Val)) Val 
nullH = proc (eq,(v1, v2)) -> do
  null1 <- op1_ -< (Null,v1)
  null2 <- op1_ -< (Null,v2)
  nulltest <- opvar_ -< (And, [null1] ++ [null2])
  ft <- if__ returnA returnA -< (isTrue nulltest, (eq, singleton Bottom))
  ff <- if__ listH returnA -< (isFalse nulltest, ((v1,v2), singleton Bottom))
  returnA -< ft ⊔ ff

listH :: (IsNum Val c, Arrow c, ArrowChoice c) => c (Val,Val) Val 
listH = proc (v1,v2) -> do 
  list1 <- op1_ -< (ListS,v1)
  list2 <- op1_ -< (ListS,v2)
  listtest <- opvar_ -< (And, [list1] ++ [list2]) 
  fft <- if__ listTH returnA -< (isTrue listtest, ((v1,v2), singleton Bottom))
  -- fst returnA -> Vector case, omitted here
  fff <- if__ returnA returnA -< (isFalse listtest, (singleton Bottom, singleton Bottom))
  returnA -< fft ⊔ fff

listTH :: (IsNum Val c, Arrow c, ArrowChoice c) => c (Val,Val) Val
listTH = proc (v1, v2) -> do
  car1 <- op1_ -< (Car, v1)
  car2 <- op1_ -< (Car, v2)
  cartest <- op2_ -< (Equal, car1, car2)
  fftt <- if__ listTTH returnA -< (isTrue cartest, ((v1,v2), singleton Bottom))
  fftf <- if__ returnA returnA -< (isFalse cartest, (singleton $ BoolVal B.False, singleton Bottom))
  returnA -< fftt ⊔ fftf

listTTH :: (IsNum Val c, Arrow c, ArrowChoice c) => c (Val,Val) Val
listTTH = proc (v1,v2) -> do 
  cdr1 <- op1_ -< (Cdr, v1)
  cdr2 <- op1_ -< (Cdr, v2)
  cdrtest <- op2_ -< (Equal, cdr1,cdr2) 
  returnA -< cdrtest
  



-- true = singleton (BoolVal B.True)
-- false = singleton (BoolVal B.False)

-- pattern True <- x | x == singleton (BoolVal B.True)
-- pattern False <- x | x == singleton (BoolVal B.False)
-- pattern Top <- x | x == singleton (BoolVal B.Top)

-- returnHelp :: c x Val 
-- returnHelp = proc _ -> returnA -< ()

if__ :: (ArrowChoice c, Complete val) => c x val -> c y val -> c (Bool,(x,y)) val
if__ f g = proc (v,(x,y)) -> case v of
  True -> f -< x
  False -> g -< y
-- inlinable

-- if__ :: Complete val => c x val -> c x val -> c (Val,x) val
-- if__ f g = proc (v,(x,y)) -> case v of
--   True -> f -< x
--   False -> g -< y
--   Top -> do 
--     -- (v1,v2) <- f *** g -< x 
--     (f -< x) <⊔> (g -< y)
--     -- returnA -< v1 ⊔ V2
-- -- inlinable 

instance (ArrowChoice c, IsString e, ArrowFail e c, ArrowComplete Val c) 
    => ArrowComplete Val (ValueT Val c) where
  ValueT f <⊔> ValueT g = ValueT $ proc x -> do
    v <- (f -< x) <⊔> (g -< x)
    case toList v of
      [Top] -> fail -< ""
      _ -> returnA -< v  

instance Hashable Addr 
instance Show Addr where 
  show (VarA (var,ctx)) = unpack var ++ show ctx
  show (CellA (expr,ctx)) = show expr ++ show ctx
  show TopA = "TopA"

instance Hashable Primitives
instance Show Primitives where
  show IntVal = "Int"
  show FloatVal = "Real"
  show (BoolVal b) = show b
  show (ClosureVal cls) = show cls
  show StringVal = "String"
  show QuoteVal = "Quote"
  show (ListVal x y) = "List [" ++ (show x) ++ ", " ++ (show y) ++ "]"
  show EmptyList = "'()"
  show Top = "Top"
  show Bottom = "Bottom"
instance Hashable ListT
instance Show ListT where
  show Nil = "Empty"
  show (Coons a1 a2) = "Cons(" ++ (show a1) ++ "," ++ (show a2) ++ ")"
  
instance IsClosure Val (HashSet Env) where
  mapEnvironment f v = case toList v of 
    [ClosureVal c] -> singleton $ ClosureVal (mapEnvironment f c)
    _ -> v
  -- TODO: fix traverseEnvironment
  traverseEnvironment _ v = case toList v of 
    [ClosureVal _] -> undefined
    _ -> pure v
  -- TODO END

-- OPERATION HELPER ------------------------------------------------------------
evalIntervalChaoticInner':: (?sensitivity :: Int) => [State Label Expr] -> (Terminating (Error (Pow String) Val))
evalIntervalChaoticInner' exprs = let res = evalIntervalChaoticInner [] exprs in (snd $ snd res)
{-# INLINE evalIntervalChaoticInner' #-}

evalIntervalChaoticOuter':: (?sensitivity :: Int) => [State Label Expr] -> (Terminating (Error (Pow String) Val))
evalIntervalChaoticOuter' exprs = let res = evalIntervalChaoticOuter [] exprs in (snd $ snd res)
{-# INLINE evalIntervalChaoticOuter' #-}

evalIntervalParallel':: (?sensitivity :: Int) => [State Label Expr] -> (Terminating (Error (Pow String) Val))
evalIntervalParallel' exprs = let res = evalIntervalParallel [] exprs in (snd $ snd res)
{-# INLINE evalIntervalParallel' #-}

evalInterval' :: (?sensitivity :: Int) => [(Text,Addr)] -> [State Label Expr] -> Terminating (Error (Pow String) Val)
evalInterval' env exprs = snd $ snd $ evalIntervalChaotic env exprs
{-# INLINE evalInterval' #-}

evalIntervalChaotic' :: (?sensitivity :: Int) => [State Label Expr] -> (Terminating (Error (Pow String) Val))
evalIntervalChaotic' exprs = let res = evalIntervalChaotic [] exprs in (snd $ snd res)
{-# INLINE evalIntervalChaotic' #-}

getCls :: [Primitives] -> [Cls]
getCls (ClosureVal cls : rest) = cls : (getCls rest) 
getCls (_:rest) = getCls rest
getCls _ = []

containsList :: [Primitives] -> Bool 
containsList [] = False 
containsList [ListVal _ _] = True
containsList [EmptyList] = True
containsList (ListVal _ _: _) = True 
containsList (EmptyList: _) = True 
containsList (_:rest) = containsList rest

allList :: [Primitives] -> Bool 
allList [] = False 
allList [ListVal _ _] = True
allList [EmptyList] = True
allList (ListVal _ _: rest) = allList rest
allList (EmptyList: rest) = allList rest
allList (_:_) = False 

carHelp :: [Primitives] -> [Addr]
carHelp [] = []
carHelp (ListVal a1 _: rest) = a1 : carHelp rest
carHelp (_:rest) = carHelp rest

cdrHelp :: [Primitives] -> [Addr]
cdrHelp [] = []
cdrHelp (ListVal _ a2: rest) = a2 : cdrHelp rest
cdrHelp (_:rest) = cdrHelp rest

-- is there any value that can be considered false?
isFalse :: Val -> Bool 
isFalse v = any (\x -> x /= BoolVal B.True && x /= Bottom) (toList v) 
        --  && any (/= Bottom) (toList v)

-- is there any value that can be considered true?
isTrue :: Val -> Bool 
isTrue v = any (\x -> x /= BoolVal B.False && x /= Bottom) (toList v)
        -- && any (/= Bottom) (toList v)


withVal :: (ArrowChoice c, ArrowFail e c, IsString e) => (Primitives -> Primitives) -> c Val Val
withVal op = proc v -> returnA -< fromList $ map op (toList v)
 
unArithmetics :: Primitives -> Primitives 
unArithmetics val = case val of 
  IntVal -> BoolVal B.Top
  FloatVal -> BoolVal B.Top
  _ -> Bottom

eqHelp :: (ArrowChoice c, ArrowFail e c, IsString e) => c (Val,Val) Val
eqHelp = proc (v1, v2) -> do
  let v1s = toList v1
  let v2s = toList v2
  case (v1s,v2s) of 
    ([EmptyList],[EmptyList]) -> returnA -< singleton $ BoolVal B.True
    ([BoolVal b1], [BoolVal b2]) -> if b1 == b2 
      then returnA -< singleton $ BoolVal B.True else returnA -< singleton $ BoolVal B.False 
    _ -> case intersect v1s v2s of 
      [] -> returnA -< singleton $ BoolVal B.False
      _ -> returnA -< singleton $ BoolVal B.Top

with2Val :: (ArrowChoice c, ArrowFail e c, IsString e) => c (Val, Val) Val
with2Val = proc (v1, v2) -> 
  case intersect (toList v1) (toList v2) of
    [IntVal] -> returnA -< fromList [IntVal]
    _ -> returnA -< singleton Bottom

withVarValNumBool :: (ArrowChoice c, ArrowFail e c, IsString e) => c [Val] Val
withVarValNumBool = proc vs -> 
  case foldl1 intersect (map toList vs) of
    [IntVal] -> returnA -< singleton $ BoolVal B.Top 
    [FloatVal] -> returnA -< singleton $ BoolVal B.Top
    [IntVal, FloatVal] -> returnA -< singleton $ BoolVal B.Top
    [FloatVal, IntVal] -> returnA -< singleton $ BoolVal B.Top
    _ -> returnA -< singleton Bottom

withVarValNumNum :: (ArrowChoice c, ArrowFail e c, IsString e) => c [Val] Val
withVarValNumNum = proc vs -> 
  case foldl1 intersect (map toList vs) of
    [IntVal] -> returnA -< singleton IntVal 
    [FloatVal] -> returnA -< singleton FloatVal 
    [IntVal, FloatVal] -> returnA -< fromList [IntVal, FloatVal]
    [FloatVal, IntVal] -> returnA -< fromList [IntVal, FloatVal]
    _ -> returnA -< singleton Bottom

withVarValNumNum' :: (ArrowChoice c, ArrowFail e c, IsString e) => c [Val] Val
withVarValNumNum' = proc vs -> 
  case foldl1 intersect (map toList vs) of
    [IntVal] -> returnA -< fromList [IntVal, FloatVal]
    [FloatVal] -> returnA -< fromList [IntVal, FloatVal] 
    [IntVal, FloatVal] -> returnA -< fromList [IntVal, FloatVal]
    [FloatVal, IntVal] -> returnA -< fromList [IntVal, FloatVal]
    _ -> returnA -< singleton Bottom

withVarValBoolBool :: (ArrowChoice c, ArrowFail e c, IsString e) => c [Val] Val
withVarValBoolBool = proc vs -> do 
  let inters = foldl1 intersect (map toList vs)
  if checkBool inters 
    then returnA -< singleton $ BoolVal $ foldl1 B.and $ valToBool inters 
    else returnA -< singleton Bottom

checkBool :: [Primitives] -> Bool
checkBool ps = case ps of
  (BoolVal _: []) -> True
  (BoolVal _:rest) -> checkBool rest
  _ -> False 
  
valToBool :: [Primitives] -> [B.Bool]
valToBool [] = []
valToBool (BoolVal b:rest) = b : (valToBool rest)
valToBool _ = [] -- should never happens
