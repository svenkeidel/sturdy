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
module IntervalAnalysis where

import           Prelude hiding (Bounded,fail,(.),exp)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Environment(extend')
import           Control.Arrow.Fix
import           Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Parallel(parallel)
import qualified Control.Arrow.Fix.Context as Ctx
import           Control.Arrow.Fix.Chaotic (iterateInner)
import           Control.Arrow.Fix.ControlFlow as CF
import           Control.Arrow.Trans
import           Control.Arrow.Closure (ArrowClosure,IsClosure(..))
import qualified Control.Arrow.Closure as Cls
import           Control.Arrow.Order
-- import           Control.Arrow.State as State
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.FiniteEnvStore
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Chaotic
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
import qualified Data.HashMap.Lazy as Map
import           Data.HashSet(HashSet)
import           Data.Graph.Inductive (Gr)
import qualified Data.Graph.Inductive as G
-- import qualified Data.HashSet as Set

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
-- import           Data.List

-- import           Data.Either

import           GHC.Exts(IsString(..))
import           GHC.Generics(Generic)
import           Text.Printf

import           Syntax (Expr(..),Literal(..) ,Op1_(..),Op2_(..),OpVar_(..), apply)
import           GenericInterpreter as Generic

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






type Cls = Closure Expr (HashSet (HashMap Text Addr))
type Addr = (Text,Ctx)
type Env = HashMap Text Addr
type Store = HashMap Addr Val
type Ctx = CallString Label


-- | Numeric values are approximated with bounded intervals, closure
-- values are approximated with a set of abstract closures.
data Val 
  = NumVal 
  | BoolVal B.Bool 
  | ClosureVal Cls 
  | StringVal
  | QuoteVal
  | ListVal Val
  | TypeError (Pow String) 
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
evalInterval :: (?sensitivity :: Int) => [(Text,Val)] -> [State Label Expr] -> Out'
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
                          (ControlFlowT Expr 
                            (->))))))))))) [Expr] Val))
    (alloc, widening)
    iterationStrategy
    (widenStore widening, T.widening (E.widening W.finite widening))
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
      CF.recordControlFlowGraph (\(_,(_,[expr])) -> expr) . 
      Fix.filter apply parallel -- iterateInner


    -- widenVal :: Widening Valtest_simple_list
    -- widenVal = widening (I.bounded ?bound)


evalInterval' :: (?sensitivity :: Int) => [(Text,Val)] -> [State Label Expr] -> Terminating (Error (Pow String) Val)
evalInterval' env exprs = snd $ snd $ snd $ evalInterval env exprs
{-# INLINE evalInterval' #-}

evalInterval'' :: (?sensitivity :: Int) => [State Label Expr] -> (Gr Expr (), Terminating (Error (Pow String) Val))
evalInterval'' exprs =
  let res = evalInterval [] exprs in (fst res, snd $ snd $ snd res)
{-# INLINE evalInterval'' #-}


instance (IsString e, ArrowChoice c, ArrowFail e c) => IsNum Val (ValueT Val c) where
  type Join y (ValueT Val c) = ArrowComplete y (ValueT Val c)

  lit = proc x -> case x of
    Number _ -> returnA -< NumVal
    Float _ -> returnA -< NumVal
    Ratio _ -> returnA -< NumVal
    Bool True  -> returnA -< BoolVal B.True 
    Bool False  -> returnA -< BoolVal B.False
    Char _ -> returnA -< StringVal
    String _ -> returnA -< StringVal
    Quote _ -> returnA -< QuoteVal
    List [] -> returnA -< TypeError "empty list"
    List (y:ys) -> case listHelp y ys of
      TypeError msg -> returnA -< TypeError msg
      val -> returnA -< ListVal val
    DottedList [] z -> returnA -< ListVal $ litsToVals z
    DottedList (y:ys) z -> returnA -< ListVal $ listHelp y (ys ++ [z])
    _ -> fail -< "(lit): Expected type didn't match with given type"

  if_ f g = proc (v,(x,y)) -> case v of
    BoolVal B.True -> f -< x 
    BoolVal B.False -> g -< y 
    BoolVal B.Top -> (f -< x) <⊔> (g -< y)
    _ -> fail -< "(if): Expected a boolean as condition"

  op1_ = proc (op, x) -> case op of
    Number_ ->
      case x of
        NumVal -> returnA -< BoolVal B.True 
        _ -> returnA -< BoolVal B.False
    Integer_ ->
      case x of
        NumVal -> returnA -< BoolVal B.Top
        _ -> returnA -< BoolVal B.False
    Float_ -> 
      case x of
        NumVal -> returnA -< BoolVal B.Top
        _ -> returnA -< BoolVal B.False
    Ratio_ -> 
      case x of
        NumVal -> returnA -< BoolVal B.Top
        _ -> returnA -< BoolVal B.False
    Zero -> 
      case x of
        NumVal -> returnA -< BoolVal B.Top
        _ -> fail -< "(zero?): Contract violation, expected element of type number"
    Positive -> 
      case x of
        NumVal -> returnA -< BoolVal B.Top
        _ -> fail -< "(positive?): Contract violation, expected element of type number"
    Negative -> 
      case x of
        NumVal -> returnA -< BoolVal B.Top
        _ -> fail -< "(negative?): Contract violation, expected element of type number"
    Odd -> 
      case x of
        NumVal -> returnA -< BoolVal B.Top
        _ -> fail -< "(odd?): Contract violation, expected element of type int: "
    Even -> 
      case x of
        NumVal -> returnA -< BoolVal B.Top
        _ -> fail -< "(even?): Contract violation, expected element of type int"
    Abs -> 
      case x of
        NumVal -> returnA -< NumVal
        _ -> fail -< "(abs): Contract violation, expected element of type int"
    Floor -> 
      case x of
        NumVal -> returnA -< NumVal
        _ -> fail -< "(floor): Contract violation, epxected elements of type number"
    Ceiling -> 
      case x of
        NumVal -> returnA -< NumVal
        _ -> fail -< "(ceiling): Contract violation, epxected elements of type number"
    Log -> 
      case x of
        NumVal -> returnA -< NumVal
        _ -> fail -< "(log): Contract violation, epxected element of type number"
    Boolean -> 
      case x of
        BoolVal _ -> returnA -< BoolVal B.True
        _ -> returnA -< BoolVal B.False
    Not -> 
      case x of
        BoolVal B.Top -> returnA -< BoolVal B.Top
        BoolVal B.True -> returnA -< BoolVal B.False
        BoolVal B.False -> returnA -< BoolVal B.True
        _ -> returnA -< BoolVal B.False
    Null -> 
      case x of
        ListVal _ -> returnA -< BoolVal B.Top
        _ -> returnA -< BoolVal B.False
    ListS -> 
      case x of
        ListVal _ -> returnA -< BoolVal B.True
        _ -> returnA -< BoolVal B.False
    Car -> 
      case x of
        ListVal n -> returnA -< n
        _ -> fail -< "(car): Bad form"
    Cdr -> 
      case x of
        ListVal n -> returnA -< ListVal n
        _ -> fail -< "(cdr): Bad form: "
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
      StringVal -> fail -< "message"
      _ -> fail -< "(fail): contract violation expected string as error msg"
  op2_ = proc (op, x, y) -> case op of
    Eqv ->
      case (x, y) of
        (BoolVal B.True, BoolVal B.True) -> returnA -< BoolVal B.True
        (BoolVal B.False, BoolVal B.False) -> returnA -< BoolVal B.True
        (BoolVal B.Top, _) -> returnA -< BoolVal B.Top
        (_, BoolVal B.Top) -> returnA -< BoolVal B.Top
        (BoolVal _, BoolVal _) -> returnA -< BoolVal B.False
        (NumVal, NumVal) -> returnA -< BoolVal B.Top
        (StringVal, StringVal) -> returnA -< BoolVal B.Top
        (QuoteVal, QuoteVal) -> returnA -< BoolVal B.Top
        _ -> returnA -< BoolVal B.False 
    Equal ->
      case (x, y) of
        (ListVal val1, ListVal val2) -> case val1 == val2 of 
          True -> returnA -< BoolVal B.Top
          False -> returnA -< BoolVal B.False
        _ -> Generic.op2_ -< (Eqv, x, y)
    Quotient ->
      case (x, y) of
        (NumVal, NumVal) -> returnA -< NumVal
        _ -> fail -< "(remainder): Contract violation, epxected elements of type int"
    Remainder ->
      case (x, y) of
        (NumVal, NumVal) -> returnA -< NumVal
        _ -> fail -< "(remainder): Contract violation, epxected elements of type int"
    Modulo ->
      case (x, y) of
        (NumVal, NumVal) -> returnA -< NumVal
        _ -> fail -< "(modulo): Contract violation, epxected elements of type int"
    Cons -> 
      case (x, y) of
        (n, ListVal val) -> returnA -< ListVal $ snd $ widening n val
        (n, m) -> returnA  -< ListVal $ snd $ widening n m
  opvar_ =  proc (op, xs) -> case op of
    EqualS -> case (withOrdHelp xs) of
      Left _ -> fail -< "(=): Contract violation, "
      Right a -> returnA -< a
    SmallerS -> case (withOrdHelp xs) of
      Left _ -> fail -< "(<): Contract violation, "
      Right a -> returnA -< a
    GreaterS -> case (withOrdHelp xs) of
      Left _ -> fail -< "(>): Contract violation, "
      Right a -> returnA -< a
    SmallerEqualS -> case (withOrdHelp xs) of
      Left _ -> fail -< "(<=): Contract violation, "
      Right a -> returnA -< a
    GreaterEqualS -> case (withOrdHelp xs) of
      Left _ -> fail -< "(>=): Contract violation, "
      Right a -> returnA -< a
    Max -> case xs of
      [] -> fail -< "(max): Arity missmatch, expected at least one argument"
      _ -> case withNumHelp xs of
        Left _ -> fail -< "(max): Contract violation, "
        Right a -> returnA -< a
    Min -> case xs of
      [] -> fail -< "(min): Arity missmatch, expected at least one argument"
      _ -> case withNumHelp xs of
        Left _ -> fail -< "(min): Contract violation, "
        Right a -> returnA -< a
    Add -> do
      case xs of
        [] -> returnA -< NumVal
        _ -> case withNumHelp xs of
          Left _ -> fail -< "(+): Contract violation, " 
          Right a -> returnA -< a
    Mul -> do
      case xs of
        [] -> returnA -< NumVal
        _ -> case withNumHelp xs of
          Left _ -> fail -< "(*): Contract violation, "
          Right a -> returnA -< a
    Sub -> do
      case xs of
        [] -> fail -< "(-): Arity missmatch, expected at least one argument"
        NumVal :[] -> returnA -< NumVal
        _ -> case withNumHelp xs of
          Left _ -> fail -< "(-): Contract violation, "
          Right a -> returnA -< a
    Div -> do
      case xs of
        [] -> fail -< "(/): Arity missmatch, expected at least one argument"
        NumVal:[] -> returnA -< NumVal
        _ -> case withNumHelp xs of
          Left _ -> fail -< "(/): Contract violation, "
          Right a -> returnA -< a
    Gcd -> case xs of
      [] -> returnA -< NumVal
      _ -> case withNumHelp xs of 
          Left _ -> fail -< "(gcd): Contract violation, "
          Right a -> returnA -< a
    Lcm -> case xs of 
      [] -> returnA -< NumVal
      _ -> case withNumHelp xs of
        Left _ -> fail -< "(lcm): Contract violation, " 
        Right a -> returnA -< a
    And -> case xs of
      [] -> returnA -< BoolVal B.True
      _ -> case withBoolAndHelp xs of 
        Left _ -> fail -< "(and): Contract violation, " 
        Right a -> returnA -< a
    Or -> case xs of 
      [] -> returnA -< BoolVal B.False
      _ -> case withBoolOrHelp xs of 
        Left _ -> fail -< "(or): Contract violation, " 
        Right a -> returnA -< a
    List_ -> returnA -< ListVal $ widenHelp xs
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
    _ -> fail -< "Expected a closure"
  {-# INLINE closure #-}
  {-# INLINE apply #-}


instance (ArrowChoice c, IsString e, ArrowFail e c, ArrowComplete Val c) => ArrowComplete Val (ValueT Val c) where
  ValueT f <⊔> ValueT g = ValueT $ proc x -> do
    v <- (f -< x) <⊔> (g -< x)
    case v of
      TypeError m -> fail -< fromString (show m)
      _           -> returnA -< v

instance PreOrd Val where
  _ ⊑ TypeError _ = True
  NumVal ⊑ NumVal = True 
  BoolVal b1 ⊑ BoolVal b2 = b1 ⊑ b2
  ClosureVal c1 ⊑ ClosureVal c2 = c1 ⊑ c2
  _ ⊑ _ = False

instance Complete Val where
  (⊔) val val' = snd $ widening val val'

instance UpperBounded Val where
  top = TypeError (singleton "Value outside the allowed range of the analysis")

instance Hashable Val
instance Show Val where
  show NumVal = "Num"
  show (BoolVal b) = show b
  show (ClosureVal cls) = show cls
  show StringVal = "String"
  show QuoteVal = "Quote123"
  show (ListVal x) = "List [" ++ (show x) ++ "]"
  show (TypeError m) = printf "TypeError: %s" (show m)
  
instance IsClosure Val (HashSet Env) where
  mapEnvironment f (ClosureVal c) = ClosureVal (mapEnvironment f c)
  mapEnvironment _ v = v
  traverseEnvironment f (ClosureVal c) = ClosureVal <$> traverseEnvironment f c
  traverseEnvironment _ v = pure v


widening :: W.Widening Val
widening NumVal NumVal = (Stable, NumVal)
widening (BoolVal x) (BoolVal y) = second BoolVal (B.widening x y)
widening (ClosureVal cs) (ClosureVal cs') = second ClosureVal $ C.widening W.finite cs cs'
widening StringVal StringVal = (Stable, StringVal)
widening QuoteVal QuoteVal = (Stable, QuoteVal)
widening (ListVal x) (ListVal y) = second ListVal (widening x y)
widening (TypeError m1) (TypeError m2) = (Stable,TypeError (m1 <> m2))
widening _ (TypeError m2) = (Unstable, TypeError m2)
widening (TypeError m1) _ = (Unstable, TypeError m1)
----------------------TRASH-------------------------------------------------------------------
widening NumVal (BoolVal _) = (Unstable, TypeError "cannot unify NumVals and BoolVals")
widening NumVal (ClosureVal _) = (Unstable, TypeError "cannot unify NumVals and ClosureVals")
widening NumVal StringVal = (Unstable, TypeError "cannot unify NumVals and StringVals")
widening NumVal QuoteVal = (Unstable, TypeError "cannot unify NumVals and QuoteVals")
widening NumVal (ListVal _) = (Unstable, TypeError "cannot unify NumVals and ListVals")

widening (BoolVal _) NumVal = (Unstable, TypeError "cannot unify BoolVals and NumVals")
widening (BoolVal _) (ClosureVal _) = (Unstable, TypeError "cannot unify BoolVals and ClosureVals")
widening (BoolVal _) StringVal = (Unstable, TypeError "cannot unify BoolVals and StringVals")
widening (BoolVal _) QuoteVal = (Unstable, TypeError "cannot unify BoolVals and QuoteVals")
widening (BoolVal _) (ListVal _) = (Unstable, TypeError "cannot unify BoolVals and ListVals")

widening (ClosureVal _) NumVal = (Unstable, TypeError "cannot unify ClosureVals and NumVals")
widening (ClosureVal _) (BoolVal _) = (Unstable, TypeError "cannot unify ClosureVals and BoolVals")
widening (ClosureVal _) StringVal = (Unstable, TypeError "cannot unify ClosureVals and StringVals")
widening (ClosureVal _) QuoteVal = (Unstable, TypeError "cannot unify ClosureVals and QuoteVals")
widening (ClosureVal _) (ListVal _) = (Unstable, TypeError "cannot unify ClosureVals and ListVals")

widening StringVal NumVal = (Unstable, TypeError "cannot unify StringVals and NumVals")
widening StringVal (BoolVal _) = (Unstable, TypeError "cannot unify StringVals and BoolVals")
widening StringVal (ClosureVal _) = (Unstable, TypeError "cannot unify StringVals and ClosureVals")
widening StringVal QuoteVal = (Unstable, TypeError "cannot unify StringVals and QuoteVals")
widening StringVal (ListVal _) = (Unstable, TypeError "cannot unify StringVals and ListVals")

widening QuoteVal NumVal = (Unstable, TypeError "cannot unify QuoteVals and NumVals")
widening QuoteVal (BoolVal _) = (Unstable, TypeError "cannot unify QuoteVals and BoolVals")
widening QuoteVal (ClosureVal _) = (Unstable, TypeError "cannot unify QuoteVals and ClosureVals")
widening QuoteVal StringVal = (Unstable, TypeError "cannot unify QuoteVals and StringVals")
widening QuoteVal (ListVal _) = (Unstable, TypeError "cannot unify QuoteVals and ListVals")

widening (ListVal _) NumVal = (Unstable, TypeError "cannot unify ListVals and NumVals")
widening (ListVal _) (BoolVal _) = (Unstable, TypeError "cannot unify ListVals and BoolVals")
widening (ListVal _) (ClosureVal _) = (Unstable, TypeError "cannot unify ListVals and ClosureVals")
widening (ListVal _) StringVal = (Unstable, TypeError "cannot unify ListVals and StringVals")
widening (ListVal _) QuoteVal = (Unstable, TypeError "cannot unify ListVals and QuoteVals")

widenStore :: Identifiable addr => Widening val -> Widening (HashMap addr val)
widenStore w m1 m2
  | Map.keys m1 == Map.keys m2 = sequenceA $ Map.intersectionWith w m1 m2
  | otherwise = (Unstable,Map.unionWith (\x y -> snd (w x y)) m1 m2)


-- OPERATION HELPER ------------------------------------------------------------
withOrdHelp :: [Val] -> Either String Val
withOrdHelp [] = Right $ BoolVal B.True
withOrdHelp (NumVal:[]) = Right $ BoolVal B.True
withOrdHelp (NumVal:NumVal:[]) = Right $ BoolVal B.Top
withOrdHelp (NumVal:xs) = withOrdHelp xs
withOrdHelp _ = Left $ "Expected elements of type ord for op"

withNumHelp :: [Val] -> Either String Val 
withNumHelp (NumVal:[]) = Right NumVal
withNumHelp (NumVal:xs) = withNumHelp xs
withNumHelp _ = Left "Expected elements of type num for op"

withBoolAndHelp :: [Val] -> Either String Val
withBoolAndHelp (BoolVal b :[]) = Right $ BoolVal b
withBoolAndHelp (BoolVal B.True: BoolVal B.True:xs) = withBoolAndHelp (BoolVal B.True:xs)
withBoolAndHelp (BoolVal _: BoolVal _:xs) = withBoolAndHelp (BoolVal B.False:xs)
withBoolAndHelp _ = Left "Expected elements of type bool for op"

withBoolOrHelp :: [Val] -> Either String Val
withBoolOrHelp (BoolVal b :[]) = Right $ BoolVal b
withBoolOrHelp (BoolVal B.False: BoolVal B.False:xs) = withBoolOrHelp (BoolVal B.False:xs)
withBoolOrHelp (BoolVal _: BoolVal _:xs) = withBoolOrHelp (BoolVal B.True:xs)
withBoolOrHelp _ = Left "Expected elements of type bool for op"

widenHelp :: [Val] -> Val
widenHelp [] = TypeError "cannot tell type from empty list"
widenHelp (x:[]) = x
widenHelp (x1:x2:xs) = widenHelp ((snd $ widening x1 x2) : xs)

listHelp :: Literal -> [Literal] -> Val
listHelp x [] = litsToVals x
listHelp x (y:[]) = snd $ widening (litsToVals x) (litsToVals y)
listHelp x (y:ys) = case snd $ widening (litsToVals x) (litsToVals y) of 
  TypeError msg -> TypeError msg
  _ -> listHelp x ys

litsToVals :: Literal -> Val
litsToVals (Number _) = NumVal
litsToVals (Float _) = NumVal
litsToVals (Ratio _) = NumVal
litsToVals (Bool True) = BoolVal B.True 
litsToVals (Bool False) = BoolVal B.False
litsToVals (Char _) = StringVal
litsToVals (String _) = StringVal
litsToVals (Quote _) = QuoteVal
litsToVals (Symbol _) = QuoteVal
litsToVals (List []) = TypeError "empty list"
litsToVals (List (n:ns)) = ListVal $ listHelp n ns
litsToVals (DottedList (n:ns) z) = ListVal $ listHelp n (ns++[z])
litsToVals (DottedList [] z) = ListVal $ litsToVals z



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
