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
module TypedAnalysis where

import           Prelude hiding (not,Bounded,fail,(.),exp)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Environment(extend')
import           Control.Arrow.Fix
import           Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Chaotic(chaotic)
import qualified Control.Arrow.Fix.Context as Ctx
-- import           Control.Arrow.Fix.Chaotic (iterateInner)
import           Control.Arrow.Fix.ControlFlow as CF
import           Control.Arrow.Trans
import           Control.Arrow.Closure (ArrowClosure,IsClosure(..))
import qualified Control.Arrow.Closure as Cls
import           Control.Arrow.Order
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.FiniteEnvStore
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Fix
-- import           Control.Arrow.Transformer.Abstract.Fix.Chaotic
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Stack
import           Control.Arrow.Transformer.Abstract.Fix.ControlFlow
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable(CacheT,Cache,Parallel,Monotone,type (**),Group)
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.Failure

import           Control.Monad.State hiding (lift,fail)

import           Data.Identifiable
import           Data.Hashable
import           Data.Label
import           Data.Order
import           Data.Text (Text)
import           Data.List
import           Data.Utils
import           Data.Profunctor
import           Data.HashMap.Lazy (HashMap)
import qualified Data.Boolean as B
import qualified Data.HashMap.Lazy as Map
import           Data.HashSet(HashSet)
import           Data.Graph.Inductive (Gr)

-- import           Data.Abstract.Boolean (Bool)
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

import           GHC.Exts(IsString(..),fromList, toList)
import           GHC.Generics(Generic)
import           Text.Printf

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
  | ListVal Primitives
  | TypeError (Pow String) 
  | Bottom
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

-- is there any value that can be considered false?
isFalse :: Val -> Bool 
isFalse v = any (== BoolVal B.False) (toList v)
-- is there any value that can be considered true?
isTrue :: Val -> Bool 
isTrue v = any (/= BoolVal B.False) (toList v)
    
instance (IsString e, ArrowChoice c, ArrowFail e c) => IsNum Val (ValueT Val c) where
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
    List [] -> returnA -< singleton $ ListVal Bottom
    -- List (y:ys) -> case listHelp y ys of
    --   TypeError msg -> fail -< fromString $ show msg
    --   val -> returnA -< ListVal val
    -- DottedList [] z -> returnA -< singleton $ ListVal $ litsToVals z
    -- DottedList (y:ys) z -> returnA -< ListVal $ listHelp y (ys ++ [z])
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
    -- Number_ -> 
    --   case x of
    --     NumVal -> returnA -< BoolVal B.True 
    --     TypeError msg -> fail -< fromString $ show msg
    --     _ -> returnA -< BoolVal B.False
    Integer_ -> withVal (\val -> case val of 
      NumVal -> BoolVal B.True 
      _ -> BoolVal B.False) -< x 
      
    -- Float_ -> withNumToTop -< x 
    -- Ratio_ -> withNumToTop -< x 
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
  --   Null -> 
  --     case x of
  --       ListVal Bottom -> returnA -< BoolVal B.True
  --       ListVal _ -> returnA -< BoolVal B.Top
  --       TypeError msg -> fail -< fromString $ show msg
  --       _ -> returnA -< BoolVal B.False
  --   ListS -> 
  --     case x of
  --       ListVal _ -> returnA -< BoolVal B.True
  --       TypeError msg -> fail -< fromString $ show msg
  --       _ -> returnA -< BoolVal B.False
  --   Car -> 
  --     case x of
  --       ListVal n -> returnA -< n
  --       TypeError msg -> fail -< fromString $ show msg
  --       _ -> fail -< fromString $ "(car): Bad form| " ++ show x
  --   Cdr -> 
  --     case x of
  --       ListVal n -> returnA -< ListVal n
  --       TypeError msg -> fail -< fromString $ show msg
  --       _ -> fail -< fromString $ "(cdr): Bad form| " ++ show x 
  --   Caar -> do
  --     v1 <- op1_ -< (Car, x)
  --     op1_ -< (Car, v1)
  --   Cadr -> do
  --     v1 <- op1_ -< (Cdr, x)
  --     op1_ -< (Car, v1)
  --   Cddr -> do
  --     v1 <- op1_ -< (Cdr, x)
  --     op1_ -< (Cdr, v1)
  --   Caddr -> do
  --     v2 <- op1_ -< (Cdr, x)
  --     op1_ -< (Car, v2)
    Error -> returnA -< singleton Bottom
  op2_ = proc (op, x, y) -> case op of
    Eqv -> eqHelp -< (x,y)
  --   Equal ->
  --     case (x, y) of
  --       (ListVal v1, ListVal v2) -> Generic.op2_ -< (Equal, v1, v2)
  --       _ -> Generic.op2_ -< (Eqv, x, y)
    Quotient -> with2Val -< (x,y) 
    Remainder -> with2Val -< (x,y)
    Modulo -> with2Val -< (x,y)
  --   Cons -> 
  --     case (x, y) of
  --       (n, ListVal val) -> returnA -< ListVal $ snd $ widening n val
  --       (TypeError msg, _) -> fail -< fromString $ show msg 
  --       (_, TypeError msg) -> fail -< fromString $ show msg
  --       (n, m) -> returnA  -< ListVal $ snd $ widening n m
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

    -- and / or not needed as it is desugared to if statements 
    -- And -> withVarValBoolBool -< xs  
    -- Or -> withVarValBoolBool -< xs 

  --   List_ -> returnA -< ListVal $ widenHelp xs
  -- {-# INLINE lit #-}
  -- {-# INLINE if_ #-}
  -- {-# INLINE op1_ #-}
  -- {-# INLINE op2_ #-}
  -- {-# INLINE opvar_ #-}

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
valToBool _ = [] -- should never happen

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
      [TypeError m] -> fail -< fromString (show m)
      -- ListVal (ListVal (ListVal (ListVal (ListVal _)))) -> fail -< "infinite list creation suspected" 
      _ -> returnA -< v  

instance PreOrd Primitives where
  _ ⊑ TypeError _ = True
  Bottom ⊑ _ = True
  NumVal ⊑ NumVal = True 
  BoolVal b1 ⊑ BoolVal b2 = b1 ⊑ b2
  ClosureVal c1 ⊑ ClosureVal c2 = c1 ⊑ c2
  StringVal ⊑ StringVal = True
  QuoteVal ⊑ QuoteVal = True 
  -- ListVal ls1 ⊑ ListVal ls2 = ls1 ⊑ ls2
  _ ⊑ _ = False

-- instance Complete Val where
--   (⊔) a b = a ⊔ b   

-- instance UpperBounded Val where
  -- top = TypeError (singleton "Value outside the allowed range of the analysis")

instance Hashable Primitives
instance Show Primitives where
  show NumVal = "Num"
  show (BoolVal b) = show b
  show (ClosureVal cls) = show cls
  show StringVal = "String"
  show QuoteVal = "Quote"
  -- show (ListVal x) = "List [" ++ (show x) ++ "]"
  show (TypeError m) = printf "TypeError: %s" (show m)
  show Bottom = "Bottom"
  
instance IsClosure Val (HashSet Env) where
  mapEnvironment f v = case toList v of 
    [ClosureVal c] -> singleton $ ClosureVal (mapEnvironment f c)
    _ -> v
  traverseEnvironment f v = case toList v of 
    [ClosureVal c] -> undefined
    _ -> pure v


-- widening :: W.Widening Val
-- widening NumVal NumVal = (Stable, NumVal)
-- widening (BoolVal x) (BoolVal y) = second BoolVal (B.widening x y)
-- widening (ClosureVal cs) (ClosureVal cs') = second ClosureVal $ C.widening W.finite cs cs'
-- widening StringVal StringVal = (Stable, StringVal)
-- widening QuoteVal QuoteVal = (Stable, QuoteVal)
-- widening (ListVal x) (ListVal y) = second ListVal (widening x y)
-- widening Bottom Bottom = (Stable, Bottom)
-- widening Bottom a = (Unstable, a) 
-- widening a Bottom = (Unstable, a)
-- widening (TypeError m1) (TypeError m2) = (Stable,TypeError (m1 <> m2))
-- widening _ (TypeError m2) = (Unstable, TypeError m2)
-- widening (TypeError m1) _ = (Unstable, TypeError m1)
-- widening a b = (Unstable, TypeError $ singleton $ "cannot unify " ++ show a ++ " and " ++ show b)
-- {-# INLINE widening #-}


widenStore :: Identifiable addr => Widening val -> Widening (HashMap addr val)
widenStore w m1 m2
  | Map.keys m1 == Map.keys m2 = sequenceA $ Map.intersectionWith w m1 m2
  | otherwise = (Unstable,Map.unionWith (\x y -> snd (w x y)) m1 m2)
{-# INLINE widenStore #-}


-- OPERATION HELPER ------------------------------------------------------------
-- withOrdHelp :: [Val] -> Either String Val
-- withOrdHelp [] = Right $ BoolVal B.True
-- withOrdHelp (NumVal:[]) = Right $ BoolVal B.True
-- withOrdHelp (NumVal:NumVal:[]) = Right $ BoolVal B.Top
-- withOrdHelp (NumVal:xs) = withOrdHelp xs
-- withOrdHelp x = Left $ fromString $ "Expected elements of type ord for op| " ++ show x
-- {-# INLINE withOrdHelp #-}

-- withNumHelp :: [Val] -> Either String Val 
-- withNumHelp (NumVal:[]) = Right NumVal
-- withNumHelp (NumVal:xs) = withNumHelp xs
-- withNumHelp x = Left $ fromString $ "Expected elements of type num for op| " ++ show x
-- {-# INLINE withNumHelp #-}

-- withBoolAndHelp :: [Val] -> Either String Val
-- withBoolAndHelp (BoolVal b :[]) = Right $ BoolVal b
-- withBoolAndHelp (BoolVal B.True: BoolVal B.True:xs) = withBoolAndHelp (BoolVal B.True:xs)
-- withBoolAndHelp (BoolVal _: BoolVal _:xs) = withBoolAndHelp (BoolVal B.False:xs)
-- withBoolAndHelp x = Left $ fromString $ "Expected elements of type bool for op| " ++ show x 
-- {-# INLINE withBoolAndHelp #-}

-- withBoolOrHelp :: [Val] -> Either String Val
-- withBoolOrHelp (BoolVal b :[]) = Right $ BoolVal b
-- withBoolOrHelp (BoolVal B.False: BoolVal B.False:xs) = withBoolOrHelp (BoolVal B.False:xs)
-- withBoolOrHelp (BoolVal _: BoolVal _:xs) = withBoolOrHelp (BoolVal B.True:xs)
-- withBoolOrHelp x = Left $ fromString $ "Expected elements of type bool for op| " ++ show x 
-- {-# INLINE withBoolOrHelp #-}

-- widenHelp :: [Val] -> Val
-- widenHelp [] = TypeError "cannot tell type from empty list"
-- widenHelp (x:[]) = x
-- widenHelp (x1:x2:xs) = widenHelp ((snd $ widening x1 x2) : xs)
-- {-# INLINE widenHelp #-}

-- listHelp :: Literal -> [Literal] -> Val
-- listHelp x [] = litsToVals x
-- listHelp x (y:[]) = snd $ widening (litsToVals x) (litsToVals y)
-- listHelp x (y:ys) = case snd $ widening (litsToVals x) (litsToVals y) of 
--   TypeError msg -> TypeError msg
--   _ -> listHelp x ys
-- {-# INLINE listHelp #-}

litsToVals :: Literal -> Val
litsToVals (Number _) = singleton NumVal
litsToVals (Float _) = singleton NumVal
litsToVals (Ratio _) = singleton NumVal
litsToVals (Bool True) = singleton $ BoolVal B.True 
litsToVals (Bool False) = singleton $ BoolVal B.False
litsToVals (Char _) = singleton $ StringVal
litsToVals (String _) = singleton StringVal
litsToVals (Quote _) = singleton QuoteVal
litsToVals (Symbol _) = singleton QuoteVal
litsToVals (List []) = singleton $ ListVal Bottom
-- litsToVals (List (n:ns)) = singleton $ ListVal $ listHelp n ns
-- litsToVals (DottedList (n:ns) z) = singleton $ ListVal $ listHelp n (ns++[z])
-- litsToVals (DottedList [] z) = singleton $ ListVal $ litsToVals z
{-# INLINE litsToVals #-}

-- withNumToTop :: (ArrowChoice c, ArrowFail e c, IsString e) => c Val Val 
-- withNumToTop = proc v -> case v of
--   NumVal -> returnA -< BoolVal B.Top
--   TypeError msg -> fail -< fromString $ show msg
--   _ -> returnA -< BoolVal B.False
-- {-# INLINE withNumToTop #-}

-- withNumToTop' :: (ArrowChoice c, ArrowFail e c, IsString e) => c Val Val 
-- withNumToTop' = proc v -> case v of
--   NumVal -> returnA -< BoolVal B.Top
--   TypeError msg -> fail -< fromString $ show msg
--   x -> fail -< fromString $ "expected value of type num| " ++ show x 
-- {-# INLINE withNumToTop' #-}

-- withNumToNum :: (ArrowChoice c, ArrowFail e c, IsString e) => c Val Val 
-- withNumToNum = proc v -> case v of
--   NumVal -> returnA -< NumVal
--   TypeError msg -> fail -< fromString $ show msg
--   x -> fail -< fromString $ "expected value of type num| " ++ show x
-- {-# INLINE withNumToNum #-}

-- withNumToNum2 :: (IsString e, ArrowChoice c, ArrowFail e c) => c (Val,Val) Val 
-- withNumToNum2 = proc v -> case v of
--   (NumVal, NumVal) -> returnA -< NumVal
--   (TypeError msg, _) -> fail -< fromString $ show msg
--   (_, TypeError msg) -> fail -< fromString $ show msg 
--   x -> fail -< fromString $ "expected values of type num| " ++ show x 
-- {-# INLINE withNumToNum2 #-}

-- withOpvarWrap :: (IsString e, ArrowChoice c, ArrowFail e c) => ([Val] -> Either String Val) -> c [Val] Val 
-- withOpvarWrap f = proc vs -> case f vs of 
--   Left msg -> fail -< fromString msg
--   Right a -> returnA -< a
-- {-# INLINE withOpvarWrap #-}

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
