{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-type-signatures #-}
-- | k-CFA analysis for PCF where numbers are approximated by intervals.
module ScalaAM where

import           Prelude hiding (not,Bounded,fail,(.),exp,zipWith)
import qualified Prelude as P 

import           Control.Category
import           Control.Arrow
import qualified Control.Arrow.Utils as ArrowUtils
import           Control.Arrow.Fail
import           Control.Arrow.Environment(extend')
import           Control.Arrow.Fix
import           Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Parallel(parallel)
import qualified Control.Arrow.Fix.Context as Ctx
import           Control.Arrow.Fix.ControlFlow as CF
import           Control.Arrow.Trans
import           Control.Arrow.Closure (ArrowClosure,IsClosure(..))
import qualified Control.Arrow.Closure as Cls
import           Control.Arrow.Order
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.FiniteEnvStore
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Stack
import           Control.Arrow.Transformer.Abstract.Fix.ControlFlow
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable(CacheT,Cache,Parallel,Monotone,type (**),Group)
import           Control.Arrow.Transformer.Abstract.Terminating

import           Control.Monad.State hiding (lift,fail)

import           Data.Identifiable
import           Data.Hashable
import           Data.Label
import           Data.Order
import           Data.Text (Text)
import           Data.Utils
import           Data.Profunctor
import           Data.HashMap.Lazy (HashMap)
import qualified Data.Boolean as B
import qualified Data.HashMap.Lazy as Map
import           Data.HashSet(HashSet)
import           Data.Graph.Inductive (Gr)

import qualified Data.Abstract.Boolean as B
import           Data.Abstract.Error (Error)
import qualified Data.Abstract.Error as E
import           Data.Abstract.Widening (Widening)
import qualified Data.Abstract.Widening as W
import           Data.Abstract.Stable
import           Data.Abstract.Terminating(Terminating)
import qualified Data.Abstract.Terminating as T
import           Data.Abstract.Closure (Closure)
import qualified Data.Abstract.Closure as C
import           Data.Abstract.DiscretePowerset (Pow)
import           Data.Abstract.CallString(CallString)

import           GHC.Exts(IsString(..),toList,fromList)
import           GHC.Generics(Generic)
import           Text.Printf

import           Syntax (Expr(..),Literal(..) ,Op1_(..),Op2_(..),OpVar_(..), apply)
import           GenericInterpreter as Generic


----------------------------- BIS 23.1.
-- | Questions:
--
-- TypeError or ListVal _ for cdr?
-- How to represent empty lists? -> ListVal Bottom
--
-- How to join Error operations?
--
-- ListVal TypeError or TypeError for lists of different types 
--
-- Wie konservativ may/must? z.B.: (lists? TypeError _) -> BoolVal B.Top oder fail 
--
-- Append to (Pow String) -> <> , overloadedlists
--
-- <> "widening (TypeError m1) (TypeError m2) = (Stable,TypeError (m1 <> m2))"
--
-- ArrowComplete?


----------------------------- BIS 27.1.
-- Export instances (Boolean) ? qualified  -> doesnt work

-- widening ⊥, representation of ⊥ in operators ?? -> correct
-- ListVal ⊥ union NonTerminating => ListVal ⊥ ?? -> correct
-- ListVal ⊥ union ListVal Num => (Stable/Unstable, ListVal Num) ?? -> correct
-- ListVal ⊥ ⊑ ListVal Num ?? -> correct 

-- recordCFG' ArrowChoice c ?? -> correct

-- showTopLevel function? -> TODO -> DONE 

-- sinnvolle Nutzung der Graphen 
-- sowie Anzahl States in GC paper?
-- ist der Anzahl an Evaluierten Exprs überhaupt sinnvoll für Vergleich von Single Env vs Multiple Envs? -> nur zum Debugging

-- unendliche listen lv lv lv lv ...
-- widening für listen -> Top -> TODO -> DONE 

-- ab wann top ? bei testfllen
-- spezifischere Fehler/TypeErrors -> TODO -> DONE

-- Grammar  Buamgramtiken 
-- non terminals unique, gleiche Sorache -> warten auf Rückmeldung

-- introd , architecture -> TODO 

-- infinite List - ArrowComplete?

----------------Curr Questions---------------------------------------------

-- Cons -> access to ?bound to use as arg in call to widening from operations 
-- solved if (⊔) works 

-- Complete W.toJoin? none terminating 

----------------BONUS-----------------------------------------
-- sinnvollere Benchmarks -> 

-- TEtSTEN, precision Erhöhen -> BONUS

-- auslagern rest of the operations  -> BONUS

-- korrekte Graphen -> BONUS


type Cls = Closure Expr (HashSet (HashMap Text Addr))
type Addr = (Text,Ctx)
type Env = HashMap Text Addr
type Store = HashMap Addr Val
type Ctx = CallString Label


-- | Numeric values are approximated with bounded intervals, closure
-- values are approximated with a set of abstract closures.
data Val 
  = IntVal (Pow Int) 
  | FloatVal (Pow Double)
  | BoolVal B.Bool 
  | ClosureVal Cls 
  | StringVal
  | QuoteVal
  | ListVal [Val]
  | TypeError (Pow String) 
  | Bottom
  deriving (Eq, Generic)

-- Input and output type of the fixpoint.
type In = (Store,(([Expr],Label),Env))
type Out = (Store, Terminating (Error (Pow String) Val))
type Out' = (Gr Expr (), ((**)
                           Monotone
                           (Parallel (Group Cache))
                           (Store, (([Expr], Label), Env))
                           (Store, Terminating (Error (Pow String) Val)),
                         (HashMap (Text, Ctx) Val, Terminating (Error (Pow String) Val))))

-- | Run the abstract interpreter for an interval analysis. The arguments are the
-- maximum interval bound, the depth @k@ of the longest call string,
-- an environment, and the input of the computation.
evalInterval :: (?sensitivity :: Int, ?bound :: Int) => [(Text,Val)] -> [State Label Expr] -> Out'
evalInterval env0 e = run (extend' (Generic.run_ ::
      Fix'
        (ValueT Val
          (ErrorT (Pow String)
            (TerminatingT
              (EnvStoreT Text Addr Val
                (FixT _ _
                  (-- ChaoticT In
                    (StackT Stack In
                      (CacheT (Monotone ** Parallel (Group Cache)) In Out
                        (ContextT Ctx
                          (ControlFlowT Expr -- unter fixT liften
                            (->))))))))))) [Expr] Val))
    (alloc, widenVal)
    iterationStrategy
    (widenStore widenVal, T.widening (E.widening W.finite widenVal))
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
      CF.recordControlFlowGraph (\(_,(_,exprs)) -> head exprs) . 
      Fix.filter apply parallel -- iterateInner

    widenVal :: Widening Val 
    widenVal = widening (numGuardTop' ?bound)


evalInterval' :: (?sensitivity :: Int, ?bound :: Int) => [(Text,Val)] -> [State Label Expr] -> Terminating (Error (Pow String) Val)
evalInterval' env exprs = snd $ snd $ snd $ evalInterval env exprs
{-# INLINE evalInterval' #-}

evalInterval'' :: (?sensitivity :: Int, ?bound :: Int) => [State Label Expr] -> (Gr Expr (), Terminating (Error (Pow String) Val))
evalInterval'' exprs =
  let res = evalInterval [] exprs in (fst res, snd $ snd $ snd res)
{-# INLINE evalInterval'' #-}


instance (IsString e, ArrowChoice c, ArrowFail e c) => IsNum Val (ValueT Val c) where
  type Join y (ValueT Val c) = ArrowComplete y (ValueT Val c)

  lit = proc x -> case x of
    Number a -> returnA -< IntVal $ singleton a
    Float f -> returnA -< FloatVal $ singleton f  
    Bool True  -> returnA -< BoolVal B.True 
    Bool False  -> returnA -< BoolVal B.False
    Char _ -> returnA -< StringVal
    String _ -> returnA -< StringVal
    Quote _ -> returnA -< QuoteVal
    List [] -> returnA -< ListVal [Bottom]
    List ys -> returnA -< ListVal $ map litsToVals ys 
    DottedList [] z -> returnA -< ListVal $ [litsToVals z]
    DottedList ys z -> returnA -< ListVal $ map litsToVals (ys ++ [z])
    _ -> fail -< fromString $ "(lit): Expected type didn't match with given type| " ++ show x 

  if_ f g = proc (v,(x,y)) -> case v of
    BoolVal B.True -> f -< x 
    BoolVal B.False -> g -< y 
    BoolVal B.Top -> (f -< x) <⊔> (g -< y)
    _ -> fail -< fromString $ "(if): Expected a boolean as condition| " ++ show v 

  op1_ = proc (op, x) -> case op of
    Number_ -> 
      case x of
        IntVal _ -> returnA -< BoolVal B.True 
        TypeError msg -> fail -< fromString $ show msg
        _ -> returnA -< BoolVal B.False
    -- start TODO 
    Integer_ -> withNumToTop -< x  
    Float_ -> withNumToTop -< x 
    Ratio_ -> withNumToTop -< x 
    Zero -> withNumToTop' -< x
    Positive -> withNumToTop' -< x 
    Negative ->  withNumToTop' -< x 
    Odd -> withNumToTop' -< x 
    Even -> withNumToTop' -< x 
    -- end TODO
    Abs -> withNumToNum abs -< x
    Floor -> withFloatToNum floor -< x
    Ceiling -> withFloatToNum ceiling -< x
    Log -> withNumToFloat log -< x 
    Boolean -> 
      case x of
        BoolVal _ -> returnA -< BoolVal B.True
        TypeError msg -> fail -< fromString $ show msg
        _ -> returnA -< BoolVal B.False
    Not -> -- js wat talk
      case x of
        BoolVal b -> returnA -< BoolVal $ B.not b
        TypeError msg -> fail -< fromString $ show msg 
        _ -> returnA -< BoolVal B.False
    Null -> 
      case x of
        ListVal [Bottom] -> returnA -< BoolVal B.True
        ListVal _ -> returnA -< BoolVal B.Top --not sure B.False might also be correct and more precise 
        TypeError msg -> fail -< fromString $ show msg
        _ -> returnA -< BoolVal B.False
    ListS -> 
      case x of
        ListVal _ -> returnA -< BoolVal B.True
        TypeError msg -> fail -< fromString $ show msg
        _ -> returnA -< BoolVal B.False
    Car -> 
      case x of
        ListVal (n:_) -> returnA -< n
        TypeError msg -> fail -< fromString $ show msg
        _ -> fail -< fromString $ "(car): Bad form| " ++ show x
    Cdr -> 
      case x of
        ListVal (_:[]) -> returnA -< ListVal [Bottom]
        ListVal (_:ns) -> returnA -< ListVal ns
        TypeError msg -> fail -< fromString $ show msg
        _ -> fail -< fromString $ "(cdr): Bad form| " ++ show x 
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
    Error -> case x of 
      StringVal -> fail -< "Scheme-Error"
      TypeError msg -> fail -< fromString $ show msg
      _ -> fail -< fromString $ "(fail): contract violation expected string as error msg| " ++ show x 
  op2_ = proc (op, x, y) -> case op of
    Eqv ->
      case (x, y) of
        (BoolVal B.True, BoolVal B.True) -> returnA -< BoolVal B.True
        (BoolVal B.False, BoolVal B.False) -> returnA -< BoolVal B.True
        (BoolVal B.Top, _) -> returnA -< BoolVal B.Top
        (_, BoolVal B.Top) -> returnA -< BoolVal B.Top
        (BoolVal _, BoolVal _) -> returnA -< BoolVal B.False
        (IntVal xs, IntVal ys) -> if length (toList xs) == 1 && length (toList ys) == 1
          then if xs == ys 
            then returnA -< BoolVal B.True 
            else returnA -< BoolVal B.Top
          else returnA -< BoolVal B.Top
        (FloatVal xs, FloatVal ys) -> if length (toList xs) == 1 && length (toList ys) == 1
          then if xs == ys 
            then returnA -< BoolVal B.True 
            else returnA -< BoolVal B.Top
          else returnA -< BoolVal B.Top
        (StringVal, StringVal) -> returnA -< BoolVal B.Top
        (QuoteVal, QuoteVal) -> returnA -< BoolVal B.Top
        (Bottom, Bottom) -> returnA -< BoolVal B.True 
        (TypeError msg, _) -> fail -< fromString $ show msg
        (_, TypeError msg) -> fail -< fromString $ show msg
        _ -> returnA -< BoolVal B.False 
    Equal ->
      case (x, y) of 
        (ListVal v1, ListVal v2) -> case v1 == v2 of -- might be wrong test!! 
          True -> returnA -< BoolVal B.True 
          False -> returnA -< BoolVal B.False 
        _ -> Generic.op2_ -< (Eqv, x, y)
    Quotient -> withNumToNum2 quot -< (x,y) 
    Remainder -> withNumToNum2 rem -< (x,y)
    Modulo -> withNumToNum2 mod -< (x,y)
    Cons -> 
      case (x, y) of
        (n, ListVal [Bottom]) -> returnA -< ListVal [n]
        (n, ListVal val) -> returnA -< ListVal (n:val)
        (TypeError msg, _) -> fail -< fromString $ show msg 
        (_, TypeError msg) -> fail -< fromString $ show msg
        (n, m) -> returnA  -< ListVal (n:m:[]) -- usually dottedlist but here '(2.3) -> '(2 3)
  opvar_ =  proc (op, xs) -> case op of
    EqualS -> withOrd (==) -< xs
    SmallerS -> withOrd (<) -< xs
    GreaterS -> withOrd (>) -< xs 
    SmallerEqualS -> withOrd (<=) -< xs 
    GreaterEqualS -> withOrd (>=) -< xs  
    Max -> case xs of
      [] -> fail -< fromString $ "(max): Arity missmatch, expected at least one argument| " ++ show xs
      _ -> withNumToNumList max -< xs  
    Min -> case xs of
      [] -> fail -< fromString $ "(min): Arity missmatch, expected at least one argument| " ++ show xs 
      _ -> withNumToNumList min -< xs  
    Add -> do
      case xs of
        [] -> returnA -< IntVal $ singleton 0
        _ -> withNumToNumList (+) -< xs 
    Mul -> do
      case xs of
        [] -> returnA -< IntVal $ singleton 1
        _ -> withNumToNumList (*) -< xs
    Sub -> do
      case xs of
        [] -> fail -< fromString $ "(-): Arity missmatch, expected at least one argument| " ++ show xs
        IntVal x:[] -> returnA -< IntVal $ fromList (map (\y -> 0 - y) (toList x))
        _ -> withNumToNumList (-) -< xs
    Div -> do
      case xs of
        [] -> fail -< fromString $ "(/): Arity missmatch, expected at least one argument" ++ show xs 
        IntVal _:[] -> returnA -< IntVal $ singleton 1 
        _ -> fail -< "doesnt work yet" 
        -- _ -> withNumToNumList (/)-< xs -- doesnt work yet 
    Gcd -> case xs of
      _ -> withNumToNumList' gcd -< xs 
    Lcm -> case xs of 
      _ -> withNumToNumList' lcm -< xs 
    And -> case xs of
      [] -> returnA -< BoolVal B.True
      _ -> withBoolToBoolList B.and -< xs  
    Or -> case xs of 
      [] -> returnA -< BoolVal B.False
      _ -> withBoolToBoolList B.or -< xs 
    List_ -> returnA -< ListVal xs 
  {-# INLINE lit #-}
  {-# INLINE if_ #-}
  {-# INLINE op1_ #-}
  {-# INLINE op2_ #-}
  {-# INLINE opvar_ #-}

instance (IsString e, ArrowChoice c, ArrowFail e c, ArrowClosure Expr Cls c)
    => ArrowClosure Expr Val (ValueT Val c) where
  type Join y Val (ValueT Val c) = Cls.Join y Cls c
  closure = ValueT $ rmap ClosureVal Cls.closure
  apply (ValueT f) = ValueT $ proc (v,x) -> case v of
    ClosureVal cls -> Cls.apply f -< (cls,x)
    _ -> fail -< fromString $ "Expected a closure| " ++ show v 
  {-# INLINE closure #-}
  {-# INLINE apply #-}
  
instance (ArrowChoice c, IsString e, ArrowFail e c, ArrowComplete Val c) => ArrowComplete Val (ValueT Val c) where
  ValueT f <⊔> ValueT g = ValueT $ proc x -> do
    v <- (f -< x) <⊔> (g -< x)
    case v of
      TypeError m -> fail -< fromString (show m)
      ListVal (_:ListVal (_:ListVal (_:ListVal (_:ListVal _:_):_):_):_) -> fail -< "infinite list creation suspected" 
      _ -> returnA -< v  

instance PreOrd Val where
  _ ⊑ TypeError _ = True
  Bottom ⊑ _ = True
  IntVal xs ⊑ IntVal ys = xs ⊑ ys
  BoolVal b1 ⊑ BoolVal b2 = b1 ⊑ b2
  ClosureVal c1 ⊑ ClosureVal c2 = c1 ⊑ c2
  StringVal ⊑ StringVal = True
  QuoteVal ⊑ QuoteVal = True 
  ListVal ls1 ⊑ ListVal ls2 = ls1 ⊑ ls2
  _ ⊑ _ = False

instance Complete Val where
  -- (⊔) = W.toJoin widening (⊔)
  (⊔) val val' = snd $ widening (numGuardTop' 1000) val val'

instance UpperBounded Val where
  top = TypeError (singleton "Value outside the allowed range of the analysis")

instance Hashable Val
instance Show Val where
  show (IntVal xs) = printf "Int: %s" (show xs)
  show (FloatVal xs) = printf "Float: %s" (show xs)
  show (BoolVal b) = show b
  show (ClosureVal cls) = show cls
  show StringVal = "String"
  show QuoteVal = "Quote"
  show (ListVal x) = "List " ++ (show x)
  show (TypeError m) = printf "TypeError: %s" (show m)
  show Bottom = "Bottom"
  
instance IsClosure Val (HashSet Env) where
  mapEnvironment f (ClosureVal c) = ClosureVal (mapEnvironment f c)
  mapEnvironment _ v = v
  traverseEnvironment f (ClosureVal c) = ClosureVal <$> traverseEnvironment f c
  traverseEnvironment _ v = pure v


widening :: Widening Val -> Widening Val
widening bound (IntVal xs) (IntVal ys) = bound (IntVal xs) (IntVal ys)
widening bound (FloatVal xs) (FloatVal ys) = bound (FloatVal xs) (FloatVal ys)
widening _ (BoolVal x) (BoolVal y) = second BoolVal (B.widening x y)
widening _ (ClosureVal cs) (ClosureVal cs') = second ClosureVal $ C.widening W.finite cs cs'
widening _ StringVal StringVal = (Stable, StringVal)
widening _ QuoteVal QuoteVal = (Stable, QuoteVal)
widening _ (ListVal xs) (ListVal ys) = case widenList xs ys of 
  [TypeError _] -> (Unstable, TypeError $ singleton $ "cannot unify lists of differing lengths| " ++ show (ListVal xs) ++ ", " ++ show (ListVal ys))
  res ->  (Stable, ListVal res) 
widening _ Bottom Bottom = (Stable, Bottom)
widening _ Bottom a = (Unstable, a) 
widening _ a Bottom = (Unstable, a)
widening _ (TypeError m1) (TypeError m2) = (Stable,TypeError (m1 <> m2))
widening _ _ (TypeError m2) = (Unstable, TypeError m2)
widening _ (TypeError m1) _ = (Unstable, TypeError m1)
widening _ a b = (Unstable, TypeError $ singleton $ "cannot unify " ++ show a ++ " and " ++ show b)
{-# INLINE widening #-}

widenStore :: Identifiable addr => Widening val -> Widening (HashMap addr val)
widenStore w m1 m2
  | Map.keys m1 == Map.keys m2 = sequenceA $ Map.intersectionWith w m1 m2
  | otherwise = (Unstable,Map.unionWith (\x y -> snd (w x y)) m1 m2)
{-# INLINE widenStore #-}

-- numGuardTop :: Int -> Val -> Val -> Val 
-- numGuardTop bound (NumVal xs) (NumVal ys) = do 
--   if any (> bound) (toList xs) || any (> bound) (toList ys)
--     then TypeError "numvals reached upperbound"
--     else NumVal (xs <> ys)
-- numGuardTop _ _ _ = TypeError "expected elem of type num"
 
-- OpVar list? 
widenList :: [Val] -> [Val] -> [Val]
widenList [] [] = [] 
widenList (x:xs) (y:ys) = if length xs == length ys 
  then ((⊔) x y): (widenList xs ys)
  else [TypeError $ singleton "trying to unify lists of differing lengths"]
widenList _ _ =[ TypeError $ singleton "error when unifying lists, should not happen"]
{-# INLINE widenList #-}

numGuardTop' :: Int -> Widening Val  
numGuardTop' bound (IntVal xs) (IntVal ys) = do 
  if any (> bound) (toList xs) || any (> bound) (toList ys)
    then (Stable, TypeError "numvals reached upperbound")
    else (Stable, IntVal (xs <> ys))
numGuardTop' bound (FloatVal xs) (FloatVal ys) = do 
  let bound' = fromIntegral bound
  if any (> bound') (toList xs) || any (> bound') (toList ys)
    then (Stable, TypeError "numvals reached upperbound")
    else (Stable, FloatVal (xs <> ys))
numGuardTop' _ _ _ = (Stable, TypeError "expected elem of type num")

-- OPERATION HELPER ------------------------------------------------------------

-- Lit
listHelp :: Literal -> [Literal] -> Val
listHelp x [] = litsToVals x
listHelp x (y:[]) = (⊔) (litsToVals x) (litsToVals y)
listHelp x (y:ys) = case (⊔) (litsToVals x) (litsToVals y) of 
  TypeError msg -> TypeError msg
  _ -> listHelp x ys
{-# INLINE listHelp #-}
--Lit
litsToVals :: Literal -> Val
litsToVals (Number x) = IntVal $ singleton x 
litsToVals (Float x) = IntVal $ singleton $ floor x 
litsToVals (Ratio x) = IntVal $ singleton $ floor x 
litsToVals (Bool True) = BoolVal B.True 
litsToVals (Bool False) = BoolVal B.False
litsToVals (Char _) = StringVal
litsToVals (String _) = StringVal
litsToVals (Quote _) = QuoteVal
litsToVals (Symbol _) = QuoteVal
litsToVals (List []) = ListVal [Bottom]
litsToVals (List xs) = ListVal $ map litsToVals xs 
litsToVals (DottedList ns z) = ListVal $ map litsToVals (ns++[z])
{-# INLINE litsToVals #-}

-- Op1  integer? float? ratio?
withNumToTop :: (ArrowChoice c, ArrowFail e c, IsString e) => c Val Val 
withNumToTop = proc v -> case v of
  IntVal _ -> returnA -< BoolVal B.Top
  TypeError msg -> fail -< fromString $ show msg
  _ -> returnA -< BoolVal B.False
{-# INLINE withNumToTop #-}
-- Op1 zero? positive? negative? odd? even?
withNumToTop' :: (ArrowChoice c, ArrowFail e c, IsString e) => c Val Val 
withNumToTop' = proc v -> case v of
  IntVal _ -> returnA -< BoolVal B.Top
  TypeError msg -> fail -< fromString $ show msg
  x -> fail -< fromString $ "expected value of type num| " ++ show x 
{-# INLINE withNumToTop' #-}
-- Op1 abs 
withNumToNum :: (ArrowChoice c, ArrowFail e c, IsString e) => (forall n. Real n => n -> n) -> c Val Val 
withNumToNum op = proc v -> case v of
  IntVal xs -> returnA -< IntVal (fromList $ map op (toList xs))
  FloatVal xs -> returnA -< FloatVal (fromList $ map op (toList xs))
  TypeError msg -> fail -< fromString $ show msg
  x -> fail -< fromString $ "expected value of type num| " ++ show x
{-# INLINE withNumToNum #-}
-- Op1 floor ceiling 
withFloatToNum :: (ArrowChoice c, ArrowFail e c, IsString e) => (Double -> Int) -> c Val Val 
withFloatToNum op = proc v -> case v of
  FloatVal xs -> returnA -< FloatVal (fromList $ map fromIntegral (map op (toList xs)))
  TypeError msg -> fail -< fromString $ show msg
  x -> fail -< fromString $ "expected value of type float| " ++ show x
{-# INLINE withFloatToNum #-}
-- Op1 log 
withNumToFloat :: (ArrowChoice c, ArrowFail e c, IsString e) => (Double -> Double) -> c Val Val 
withNumToFloat op = proc v -> case v of
  IntVal xs -> returnA -< FloatVal $ fromList $ map op (map fromIntegral $ toList xs)
  FloatVal xs -> returnA -< FloatVal $ fromList $ map op (toList xs)
  TypeError msg -> fail -< fromString $ show msg
  x -> fail -< fromString $ "expected value of type num| " ++ show x
{-# INLINE withNumToFloat #-}

-- Op2 quotient? remainder? modulo?
withNumToNum2 :: (IsString e, ArrowChoice c, ArrowFail e c) => (Int -> Int -> Int) -> c (Val, Val) Val  
withNumToNum2 op = proc (v1, v2) -> case (v1, v2) of
  (IntVal xs, IntVal ys) -> returnA -< IntVal (fromList $ map (\(x,y) -> op x y) [(x,y) | x <- (toList xs), y <- (toList ys)])
  _ -> fail -< fromString $ "expected two values of type num| " ++ show v1 ++ ", " ++ show v2 
{-# INLINE withNumToNum2 #-}

-- OpVar = < > <= >=
withOrd :: (ArrowChoice c, ArrowFail e c, IsString e) => (forall a. (Ord a, Eq a) => a -> a -> Bool) -> c [Val] Val 
withOrd op = proc vs -> 
  case checkLength vs of 
    Left "Top" -> returnA -< BoolVal B.Top
    Right 1 -> do 
      tmps <- ArrowUtils.map numToReal -< vs 
      case withOrderHelp op True (zip tmps (tail tmps)) of 
        Right True -> returnA -< BoolVal B.True 
        Right False -> returnA -< BoolVal B.False
        Left msg -> fail -< fromString msg
    _ -> fail -< fromString $ "Expected elements of typ num for op| " ++ show vs 
-- OpVar max min + * - 
withNumToNumList :: (IsString e, ArrowChoice c, ArrowFail e c) => (forall a. (Ord a, Num a) => a -> a -> a) -> c [Val] Val 
withNumToNumList op = proc vs -> case vs of 
  (IntVal _:_) -> do
    xss <- ArrowUtils.map intValToInt -< vs 
    case (fromList $ zipWithNum op xss) of 
      [] -> fail -< fromString $ "didn't expect empty list"
      res -> returnA -< IntVal res
  (FloatVal _:_) -> do
    xss <- ArrowUtils.map numToReal -< vs 
    case (fromList $ zipWithNum op xss) of 
      [] -> fail -< fromString $ "didn't expect empty list"
      res -> returnA -< FloatVal res
  _ -> fail -< fromString $ "Expected elements of typ num for op| " ++ show vs 
{-# INLINE withNumToNumList #-}
-- OpVar gcd lcm
withNumToNumList' :: (IsString e, ArrowChoice c, ArrowFail e c) => (forall a. (Integral a) => a -> a -> a) -> c [Val] Val 
withNumToNumList' op = proc vs -> case vs of 
  (IntVal _:_) -> do
    xss <- ArrowUtils.map intValToInt -< vs 
    case (fromList $ zipWithNum op xss) of 
      [] -> fail -< fromString $ "didn't expect empty list"
      res -> returnA -< IntVal res
  _ -> fail -< fromString $ "Expected elements of typ int for op| " ++ show vs 
{-# INLINE withNumToNumList' #-}
-- OpVar and or
withBoolToBoolList :: (IsString e, ArrowChoice c, ArrowFail e c) => (B.Bool -> B.Bool -> B.Bool) -> c [Val] Val 
withBoolToBoolList op = proc vs -> do 
  tmps <- ArrowUtils.map valToBool -< vs 
  returnA -< BoolVal $ foldl op (head tmps) tmps 
{-# INLINE withBoolToBoolList #-}


--------------------HELPER HELPER----------------------------------------------------------------

-- withOrd
checkLength :: [Val] -> Either String Int
checkLength (IntVal x:[]) = case length $ toList x of 
  1 -> Right 1
  _ -> Left "Top"
checkLength (FloatVal x:[]) = case length $ toList x of 
  1 -> Right 1
  _ -> Left "Top"  
checkLength (IntVal x:xs) = case length $ toList x of 
  1 -> checkLength xs
  _ -> Left "Top" 
checkLength (FloatVal x:xs) = case length $ toList x of 
  1 -> checkLength xs
  _ -> Left "Top" 
checkLength _ = Left "Fail"
{-# INLINE checkLength #-}

--withOrd
withOrderHelp :: (Ord a) => (a -> a -> P.Bool) -> P.Bool -> [([a], [a])] -> Either String P.Bool
withOrderHelp op b (([v1],[v2]):[]) = Right $ b && (op v1 v2)
withOrderHelp op b (([v1],[v2]):vs) = withOrderHelp op (b && (op v1 v2)) vs
withOrderHelp _ _ _ = Left $ "Expected elements of type num and card 1 for op| " -- ++ show vs
{-# INLINE withOrderHelp #-}

-- withNumToNumList
zipWithNum :: (a -> a -> a) -> [[a]] -> [a] 
zipWithNum _ [] = []
zipWithNum _ (as:[]) = as 
zipWithNum op (as:bs:xss) = do
  let tmps = [op x1 x2 | x1 <- as, x2 <- bs]
  zipWithNum op (tmps:xss) 
{-# INLINE zipWithNum #-}


-- withNumToNumList withOrd
numToReal :: (IsString e, ArrowChoice c, ArrowFail e c) => c Val [Double]
numToReal = proc v -> case v of 
  IntVal xs -> returnA -< map fromIntegral (toList xs)
  FloatVal xs -> returnA -< (toList xs) 
  _ -> fail -< fromString $ "expected value typ int| " ++ show v 
{-# INLINE numToReal #-}

intValToInt :: (IsString e, ArrowChoice c, ArrowFail e c) => c Val [Int]
intValToInt = proc v -> case v of 
  IntVal xs -> returnA -< (toList xs)
  _ -> fail -< fromString $ "expected value typ int| " ++ show v 
{-# INLINE intValToInt #-}


-- withBoolToBoolList
valToBool :: (IsString e, ArrowChoice c, ArrowFail e c) => c Val B.Bool 
valToBool = proc v -> case v of 
  BoolVal b -> returnA -< b 
  _ -> fail -< fromString $ "expected element of type bool| " ++ show v 
{-# INLINE valToBool #-}

-- instance Traversable Val where
--   traverse _ (NumVal n) = pure $ NumVal n
--   traverse _ (TypeError e) = pure $ TypeError e
--   traverse f (ClosureVal c) = ClosureVal <$> traverse f c

-- instance Foldable Val where
--   foldMap = foldMapDefault

-- type Cls = Closure Expr (HashSet (HashMap Text Addr))
-- data Val = NumVal IV | ClosureVal Cls | TypeError (Pow String) deriving (Eq, Generic)

-- collect :: FixpointCombinator c (((Expr,Label),(HashMap Text Addr,HashMap Addr Val))) b
-- collect :: (ArrowState Store c) => FixpointCombinator c ((Expr, Label), Env, Store) b 
-- type In = (Store,((Expr,Label),Env))

-- collect :: (ArrowState Store c) => FixpointCombinator c a b 
-- collect f = proc a -> do
--   cache <- State.get -< ()  -- get curr store of input to computation
--   --let store' = garbage_collect store  -- compute all reachable addresses and remove non-reachables addresses 
--   State.put -< garbage_collect cache  -- set curr store to modified garbage free store
--   f -< a  -- continue computation 

-- -- gets a store and returns a store with unreferenced addrs removed
-- garbage_collect :: HashMap Addr Val -> HashMap Addr Val
-- garbage_collect store =
--   let addrs = Map.keys store  -- get addrs currently used in store 
--       referenced_addrs = get_referenced_addrs $ Map.elems store  -- get all addrs referenced by the curr state
--       collect_addrs = addrs \\ referenced_addrs  -- all addrs that are in store and are not referenced 
--   in remove_addrs collect_addrs store  -- return store with all addrs to be collected removed

-- -- retrieve referenced addrs from values 
-- -- those addrs are the ones that are accessible from some env of some closure
-- get_referenced_addrs :: [Val] -> [Addr] 
-- get_referenced_addrs ((ClosureVal cls): xs) = get_addrs cls ++ get_referenced_addrs xs
-- get_referenced_addrs (_:xs) = get_referenced_addrs xs 
-- get_referenced_addrs [] = [] 

-- -- TODO: Fix flattening (remove head)
-- -- retrieve addrs reference by specific closure 
-- get_addrs :: Closure Expr (HashSet (HashMap Text Addr)) -> [Addr]
-- get_addrs cls = Map.elems $ head $ Set.toList $ C.get_env cls 

-- -- remove addrs from a store and return  modified store 
-- remove_addrs :: [Addr] -> HashMap Addr Val -> HashMap Addr Val
-- remove_addrs (x:xs) store = remove_addrs xs (Map.delete x store)
-- remove_addrs [] store = store
