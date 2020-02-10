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
import qualified Control.Arrow.Utils as ArrowUtils
import           Control.Arrow.Fail
import           Control.Arrow.Environment
import           Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Chaotic(chaotic)
import qualified Control.Arrow.Fix.Context as Ctx
-- import           Control.Arrow.Fix.Chaotic (iterateInner)
-- import           Control.Arrow.Fix.ControlFlow as CF
import           Control.Arrow.Trans
import           Control.Arrow.Closure (ArrowClosure,IsClosure(..))
import qualified Control.Arrow.Closure as Cls
import           Control.Arrow.Order
import           Control.Arrow.Store
import qualified Control.Arrow.Store as Store

-- import Control.Arrow.State as State
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.FiniteEnvStore
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Fix
-- import           Control.Arrow.Transformer.Abstract.Fix.Chaotic
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Stack
-- import           Control.Arrow.Transformer.Abstract.Fix.ControlFlow
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable(CacheT,Monotone)
import           Control.Arrow.Transformer.Abstract.Terminating
-- import           Control.Arrow.Transformer.Abstract.Failure

import           Control.Monad.State hiding (lift,fail)

import           Data.Identifiable
import           Data.Hashable
import           Data.Label
import           Data.Order
import           Data.Text (Text, pack, intercalate)
import           Data.List (intersect)
import           Data.Utils
-- import           Data.Profunctor
import           Data.HashMap.Lazy (HashMap)
import qualified Data.Boolean as B
import qualified Data.HashMap.Lazy as Map
import           Data.HashSet(HashSet)
-- import           Data.Graph.Inductive (Gr)

-- import           Data.Abstract.Boolean (Bool)
import qualified Data.Abstract.Boolean as B
import           Data.Abstract.Error (Error)
-- import qualified Data.Abstract.Error as E
import           Data.Abstract.Widening (Widening)
import qualified Data.Abstract.Widening as W
import           Data.Abstract.Stable
import           Data.Abstract.Terminating(Terminating)
-- import qualified Data.Abstract.Terminating as T
import           Data.Abstract.Closure (Closure)
-- import qualified Data.Abstract.Closure as C
import           Data.Abstract.DiscretePowerset (Pow)
import           Data.Abstract.CallString(CallString)

import           GHC.Exts(IsString(..),fromList, toList)
import           GHC.Generics(Generic)
-- import           Text.Printf

import           Syntax (Expr(..),Literal(..) ,Op1_(..),Op2_(..),OpVar_(..), apply)
import           GenericInterpreter as Generic

type Cls = Closure Expr (HashSet (HashMap Text Addr))
type Addr = (Text,Ctx)
type Env = HashMap Text Addr
type Store = HashMap Addr Val
type Ctx = CallString Label


-- | Numeric values are approximated with bounded intervals, closure
-- values are approximated with a set of abstract closures.
type Val = Pow Primitives
data Primitives
  = NumVal 
  | BoolVal B.Bool 
  | ClosureVal Cls 
  | StringVal
  | QuoteVal
  | ListVal ListT
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
-- type Out' = (Gr Expr (), ((**)
--                            Monotone
--                            (Parallel (Group Cache))
--                            (Store, (([Expr], Label), Env))
--                            (Store, Terminating (Error (Pow String) Val)),
--                          (HashMap (Text, Ctx) Val, Terminating (Error (Pow String) Val))))
type Out' = (--Gr Expr (),
                        (Monotone
                           (Store, (([Expr], Label), Env))
                           (Store, Terminating (Error (Pow String) Val)),
                         (HashMap (Text, Ctx) Val, Terminating (Error (Pow String) Val))))
-- | Run the abstract interpreter for an interval analysis. The arguments are the
-- maximum interval bound, the depth @k@ of the longest call string,
-- an environment, and the input of the computation.
evalInterval :: (?sensitivity :: Int) => [(Text,Val)] -> [State Label Expr] -> Out'
evalInterval env0 e = run (extend' (Generic.run_ ::
      Fix'
        (ValueT Val
          (ErrorT (Pow String)
            (TerminatingT
              (EnvStoreT Text Addr Val
                (FixT _ _
                  (--FailureT Val
                    (--ChaoticT In
                      (StackT Stack In
                        (CacheT Monotone In Out
                          (ContextT Ctx 
                            (--ControlFlowT Expr -- unter fixT liften
                              (->)))))))))))) [Expr] Val))
    (alloc, W.finite)
    iterationStrategy
    (W.finite, W.finite)
    (Map.empty,(Map.empty,(env0,e0)))
  where
    e0 = generate (sequence e)

    alloc = proc (var,_) -> do
      ctx <- Ctx.askContext @Ctx -< ()
      returnA -< (var,ctx)

    iterationStrategy =
      -- Fix.traceShow .
      -- collect . 
      Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of [App _ _ l] -> Just l; _ -> Nothing) .
      -- CF.recordControlFlowGraph' (\(_,(_,exprs)) -> case exprs of [App x y z] -> Just (App x y z); _ -> Nothing) . 
      -- CF.recordControlFlowGraph (\(_,(_,exprs)) -> head exprs) . 
      Fix.filter apply chaotic -- parallel -- iterateInner



evalInterval' :: (?sensitivity :: Int) => [(Text,Val)] -> [State Label Expr] -> Terminating (Error (Pow String) Val)
-- evalInterval' env exprs = snd $ snd $ snd $ evalInterval env exprs
evalInterval' env exprs = snd $ snd $ evalInterval env exprs

{-# INLINE evalInterval' #-}

-- evalInterval'' :: (?sensitivity :: Int) => [State Label Expr] -> (Gr Expr (), Terminating (Error (Pow String) Val))
evalInterval'' :: (?sensitivity :: Int) => [State Label Expr] -> (Terminating (Error (Pow String) Val))
evalInterval'' exprs =
  let res = evalInterval [] exprs in (snd $ snd res)
{-# INLINE evalInterval'' #-}
    
instance (Store.Join Val c, IsString e, ArrowChoice c, ArrowFail e c, ArrowStore Text Val c) => IsNum Val (ValueT Val c) where
  type Join y (ValueT Val c) = ArrowComplete y (ValueT Val c)

  lit = proc x -> case x of
    Number _ -> returnA -< singleton NumVal
    Float _ -> returnA -< singleton NumVal
    Ratio _ -> returnA -< singleton NumVal
    Bool True  -> returnA -< singleton $ BoolVal B.True 
    Bool False  -> returnA -< singleton $ BoolVal B.False
    Char _ -> returnA -< singleton StringVal
    String _ -> returnA -< singleton StringVal
    Quote _ -> returnA -< singleton QuoteVal
    List ys -> do
      let vars = map pack $ map litsToString ys
      let vals = map singleton $ map litsToVals ys
      ArrowUtils.map write -< zip vars vals
      returnA -< singleton $ ListVal $ Coons (head vars) (intercalate " " (tail vars))
    DottedList _ _ -> returnA -< singleton $ litsToVals x
    _ -> returnA -< singleton Bottom

  if_ f g = proc (v,(x,y)) ->
    if isTrue v && isFalse v
      then  (f -< x) <⊔> (g -< y)
      else if isTrue v
        then f -< x
        else if isFalse v
          then g -< y 
          else fail -< "if: should not happen"

  op1_ = proc (op, x) -> case op of
    Number_ -> withVal (\val -> case val of 
      NumVal -> BoolVal B.True
      _ -> BoolVal B.False) -< x
    Integer_ -> withVal (\val -> case val of 
      NumVal -> BoolVal B.True 
      _ -> BoolVal B.False) -< x 
    -- TODO : Change when adding FloatVal 
    Float_ -> withVal (\val -> case val of 
      NumVal -> BoolVal B.True 
      _ -> BoolVal B.False) -< x 
    Ratio_ -> withVal (\val -> case val of 
      NumVal -> BoolVal B.True 
      _ -> BoolVal B.False) -< x 
    -- END TODO
    Zero -> withVal unArithmetics -< x
    Positive -> withVal unArithmetics -< x 
    Negative -> withVal unArithmetics -< x 
    Odd -> withVal unArithmetics -< x 
    Even -> withVal unArithmetics -< x 
    Abs -> withVal unArithmetics -< x
    Floor -> withVal unArithmetics -< x
    Ceiling -> withVal unArithmetics -< x 
    Log -> withVal unArithmetics -< x 
    Boolean -> withVal (\val -> case val of 
      BoolVal _ -> BoolVal B.True
      _ -> BoolVal B.False) -< x 
    Not -> withVal (\val -> case val of -- js wat talk
      BoolVal b -> BoolVal $ B.not b
      _ -> Bottom) -< x 
    Null -> withVal (\val -> case val of 
      ListVal Nil -> BoolVal B.True
      _ -> BoolVal B.False) -< x        
    ListS -> returnA -< singleton Bottom
    Car -> case toList x of
        [ListVal (Coons a1 _)] -> do 
          v <- read' -< a1 
          returnA -< v
        _ -> returnA -< singleton Bottom
    Cdr -> returnA -< undefined
    Caar -> returnA -< undefined
    Cadr -> returnA -< singleton Bottom
    Cddr ->returnA -< singleton Bottom
    Caddr ->returnA -< singleton Bottom
    Error -> returnA -< singleton Bottom
  op2_ = proc (op, x, y) -> case op of
    Eqv -> eqHelp -< (x,y)
    Equal -> returnA -< undefined
    Quotient -> with2Val -< (x,y) 
    Remainder -> with2Val -< (x,y)
    Modulo -> with2Val -< (x,y)
    Cons -> returnA -< undefined
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
    Div -> withVarValNumNum -< xs 
    Gcd -> withVarValNumNum -< xs 
    Lcm -> withVarValNumNum -< xs 
    List_ -> do
      let vars = map pack $ map primitivesToStrings (map toList xs)
      ArrowUtils.map write -< (zip vars xs)
      returnA -< singleton $ ListVal $ Coons (head vars) (head $ tail vars)
  -- {-# INLINE lit #-}
  -- {-# INLINE if_ #-}
  -- {-# INLINE op1_ #-}
  -- {-# INLINE op2_ #-}
  -- {-# INLINE opvar_ #-}

primitivesToStrings :: [Primitives] -> String 
primitivesToStrings ps = case ps of 
  [] -> ""
  (NumVal: rest) -> "NV" ++ (primitivesToStrings rest)
  BoolVal b:rest -> show b ++ (primitivesToStrings rest)
  ClosureVal _:rest -> "ClsV" ++ (primitivesToStrings rest)
  StringVal:rest -> "StrV" ++ (primitivesToStrings rest)
  QuoteVal:rest -> "QV" ++ (primitivesToStrings rest)
  ListVal _:rest -> "LV" ++ (primitivesToStrings rest)
  -- ListVal ls:rest -> "LV(" ++ (primitivesToStrings ls) ++ ")" ++ (primitivesToStrings rest)
  Top:rest -> "Top" ++ (primitivesToStrings rest)
  Bottom:rest -> "Bot" ++ (primitivesToStrings rest)

instance (IsString e, ArrowChoice c, ArrowFail e c, ArrowClosure Expr Cls c)
    => ArrowClosure Expr Val (ValueT Val c) where
  type Join y Val (ValueT Val c) = Cls.Join y Cls c
  closure = ValueT $ proc e -> do 
    cls <- Cls.closure -< e
    returnA -< singleton (ClosureVal cls)
  apply (ValueT f) = ValueT $ proc (v,x) -> case head $ toList v of
    ClosureVal cls -> Cls.apply f -< (cls,x)
    _ -> fail -< fromString $ "Expected a closure| " ++ show v 
  {-# INLINE closure #-} 
  {-# INLINE apply #-}
  
instance (ArrowChoice c, IsString e, ArrowFail e c, ArrowComplete Val c) 
    => ArrowComplete Val (ValueT Val c) where
  ValueT f <⊔> ValueT g = ValueT $ proc x -> do
    v <- (f -< x) <⊔> (g -< x)
    case toList v of
      [Top] -> fail -< ""
      -- ListVal (ListVal (ListVal (ListVal (ListVal _)))) -> fail -< "infinite list creation suspected" 
      _ -> returnA -< v  

-- TODO: Fix for lists
instance PreOrd Primitives where
  _ ⊑ Top = True
  Bottom ⊑ _ = True
  NumVal ⊑ NumVal = True 
  BoolVal b1 ⊑ BoolVal b2 = b1 ⊑ b2
  ClosureVal c1 ⊑ ClosureVal c2 = c1 ⊑ c2
  StringVal ⊑ StringVal = True
  QuoteVal ⊑ QuoteVal = True 
  -- ListVal ls1 ⊑ ListVal ls2 = ls1 ⊑ ls2
  _ ⊑ _ = False
-- END TODO

instance Hashable Primitives
instance Show Primitives where
  show NumVal = "Num"
  show (BoolVal b) = show b
  show (ClosureVal cls) = show cls
  show StringVal = "String"
  show QuoteVal = "Quote"
  show (ListVal x) = "List [" ++ (show x) ++ "]"
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

widenStore :: Identifiable addr => Widening val -> Widening (HashMap addr val)
widenStore w m1 m2
  | Map.keys m1 == Map.keys m2 = sequenceA $ Map.intersectionWith w m1 m2
  | otherwise = (Unstable,Map.unionWith (\x y -> snd (w x y)) m1 m2)
{-# INLINE widenStore #-}


-- OPERATION HELPER ------------------------------------------------------------

-- is there any value that can be considered false?
isFalse :: Val -> Bool 
isFalse v = elem (BoolVal B.False) (toList v)
-- is there any value that can be considered true?
isTrue :: Val -> Bool 
isTrue v = any (/= BoolVal B.False) (toList v)

withVal :: (ArrowChoice c, ArrowFail e c, IsString e) => (Primitives -> Primitives) -> c Val Val
withVal op = proc v -> returnA -< fromList $ map op (toList v)
 
unArithmetics :: Primitives -> Primitives 
unArithmetics val = case val of 
  NumVal -> BoolVal B.Top
  _ -> Bottom

eqHelp :: (ArrowChoice c, ArrowFail e c, IsString e) => c (Val,Val) Val
eqHelp = proc (v1, v2) -> do
  let v1s = toList v1
  let v2s = toList v2
  case (v1s,v2s) of 
    ([BoolVal b1], [BoolVal b2]) -> if b1 == b2 
      then returnA -< singleton $ BoolVal B.True else returnA -< singleton $ BoolVal B.False 
    _ -> case intersect v1s v2s of 
      [] -> returnA -< singleton $ BoolVal B.False
      _ -> returnA -< singleton $ BoolVal B.Top

with2Val :: (ArrowChoice c, ArrowFail e c, IsString e) => c (Val, Val) Val
with2Val = proc (v1, v2) -> 
  case intersect (toList v1) (toList v2) of
    [NumVal] -> returnA -< singleton NumVal 
    _ -> returnA -< singleton Bottom

withVarValNumBool :: (ArrowChoice c, ArrowFail e c, IsString e) => c [Val] Val
withVarValNumBool = proc vs -> 
  case foldl1 intersect (map toList vs) of
    [NumVal] -> returnA -< singleton $ BoolVal B.Top 
    _ -> returnA -< singleton Bottom

withVarValNumNum :: (ArrowChoice c, ArrowFail e c, IsString e) => c [Val] Val
withVarValNumNum = proc vs -> 
  case foldl1 intersect (map toList vs) of
    [NumVal] -> returnA -< singleton NumVal 
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

litsToVals :: Literal -> Primitives
litsToVals (Number _) = NumVal
litsToVals (Float _) = NumVal
litsToVals (Ratio _) = NumVal
litsToVals (Bool True) = BoolVal B.True 
litsToVals (Bool False) = BoolVal B.False
litsToVals (Char _) = StringVal
litsToVals (String _) = StringVal
litsToVals (Quote _) = QuoteVal
litsToVals (Symbol _) = QuoteVal
litsToVals (List []) = ListVal Nil
litsToVals (List _) = undefined
litsToVals (DottedList _ _) = undefined
-- litsToVals (DottedList [] z) = singleton $ ListVal $ [litsToVals z]
{-# INLINE litsToVals #-}


litsToString :: Literal -> String 
litsToString ls = case ls of 
  (Number _ ) -> "NV"
  (Float _ ) -> "NV"
  (Ratio _ ) -> "NV"
  Bool True -> "True"
  Bool False -> "False"
  Char _ -> "StrV"
  String _ -> "StrV"
  Quote _ -> "QV"
  List _ -> "LV"
  Symbol _ -> "SV"
  -- List ls:rest -> "LV(" ++ (litsToString ls) ++ ")" ++ (litsToString rest)
  DottedList _ _ -> "LV"