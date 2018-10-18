{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -fno-warn-orphans #-}
module SortSemantics where

import           Prelude hiding ((.),fail)

import           SharedSemantics
import           Sort (SortId(..))
import           Syntax hiding (Fail,TermPattern(..))
import           Utils

import           Control.Arrow
import           Control.Arrow.Deduplicate
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Transformer.Abstract.HandleExcept
import           Control.Arrow.Transformer.Abstract.Powerset
import           Control.Arrow.Transformer.Abstract.GreatestFixPoint
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Category

import qualified Data.Abstract.Powerset as A
import           Data.Abstract.HandleError
import           Data.Constructor
import           Data.Foldable (foldr')
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.List(intercalate)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Hashable
import           Data.Order
import           Data.Text (unpack)

import           GHC.Generics (Generic)

data Sort = Bottom | Top | Lexical | Numerical | Option Sort | List Sort | Tuple [Sort] | Sort SortId
  deriving (Eq, Ord, Generic)

data SortContext = SortContext {
  signatures :: HashMap Constructor ([Sort], Sort)
, lexicals :: Set Sort
, injections :: Set (Sort, Sort)
, injectionClosure :: HashMap Sort (Set Sort)
, reverseInjectionClosure :: HashMap Sort (Set Sort)
} deriving (Eq)

data Term = Term {
  sort :: Sort
, context :: SortContext
} deriving (Eq)

isLexical :: Term -> Bool
isLexical (Term Lexical _) = True
isLexical (Term s ctx) = Set.member s (lexicals ctx)

isList :: Term -> Bool
isList (Term (List _) _) = True
isList _ = False

newtype TermEnv = TermEnv (HashMap TermVar Term) deriving (Show,Eq,Hashable)





-- |
newtype Interp a b = Interp (Reader (StratEnv,SortContext) (State TermEnv (Except () (Powerset GreatestFixPoint))) a b)
  deriving (Category,Arrow,ArrowChoice,ArrowApply,PreOrd,Complete)

runInterp :: Interp a b -> Int -> StratEnv -> SortContext -> TermEnv -> a -> A.Pow (Error () (TermEnv,b))
runInterp (Interp f) k senv sig tenv a =
  runGreatestFixPoint k
    (runPowerset
      (runExcept
        (runState
          (runReader f))))
    (tenv, ((senv, sig), a))

eval :: Int -> Strat -> StratEnv -> SortContext -> TermEnv -> Term -> A.Pow (Error () (TermEnv,Term))
eval i s = runInterp (eval' s) i

emptyEnv :: TermEnv
emptyEnv = TermEnv M.empty

-- Instances -----------------------------------------------------------------------------------------
deriving instance ArrowReader (StratEnv, SortContext) Interp
deriving instance ArrowState TermEnv Interp
deriving instance ArrowFix (Strat,Term) Term Interp
deriving instance PreOrd y => ArrowExcept x y () Interp
deriving instance ArrowDeduplicate Term Term Interp

instance ArrowFail () Interp where
  fail = Interp fail

instance HasStratEnv Interp where
  readStratEnv = proc _ -> do
    (env,_) <- ask -< ()
    returnA -< env
  localStratEnv senv f = proc a -> do
    (_,ctx) <- ask -< ()
    r <- local f -< ((senv,ctx),a)
    returnA -< r

withSortContext :: ArrowReader (StratEnv, SortContext) c => (a -> SortContext -> b) -> c a b
withSortContext f = proc a -> do
  (_,ctx) <- ask -< ()
  returnA -< f a ctx

instance IsTermEnv TermEnv Term Interp where
  getTermEnv = get
  putTermEnv = put
  lookupTermVar f g = proc (v,TermEnv env) ->
    case M.lookup v env of
      Just t -> f -< t
      Nothing ->
        (proc () -> do
          putTermEnv -< TermEnv (M.insert v top env)
          f -< top)
        ⊔ g
        -<< ()
  insertTerm = arr $ \(v,t,TermEnv env) -> TermEnv (M.insert v t env)
  deleteTermVars = arr $ \(vars,TermEnv env) -> TermEnv (foldr' M.delete env vars)
  unionTermEnvs = arr (\(vars,TermEnv e1,TermEnv e2) -> TermEnv (M.union e1 (foldr' M.delete e2 vars)))


instance IsTerm Term Interp where
  matchTermAgainstConstructor matchSubterms = proc (c,ts,Term termSort ctx) -> do
    let (patParams,patSort) = signatures ctx M.! c
    if eqLength patParams ts && Term patSort ctx ⊑ Term termSort ctx
      then do matchSubterms -< (ts,map (\s -> Term s ctx) patParams)
              returnA ⊔ fail' -< Term patSort ctx
      else fail -< ()

  matchTermAgainstString = proc (_,t) ->
    if isLexical t
      then returnA ⊔ fail' -< t
      else fail -< ()

  matchTermAgainstNumber = proc (_,t@(Term termSort _)) -> case termSort of
    Numerical -> returnA ⊔ fail' -< t
    _ -> fail -< ()

  matchTermAgainstExplode matchCons matchSubterms = proc t -> case t of
    _ | isLexical t -> do
      matchSubterms -< convertToList [] (context t)
      returnA -< t
    Term Numerical ctx -> do
      matchSubterms -< convertToList [] ctx
      returnA -< t
    Term _ ctx -> do
      matchCons -< Term Lexical ctx
      matchSubterms -< Term (List Top) ctx
      returnA -< t

  equal = proc (t1,t2) ->
    if t1 ⊑ t2 || t2 ⊑ t1
      then returnA ⊔ fail' -< t1
      else fail -< ()

  convertFromList = proc (t,ts) ->
    if isLexical t && isList ts
      then returnA ⊔ fail' -< Term Top (context t) -- cannot deduct target sort from sort Lexical
      else fail -< ()

  -- Sorts don't have "subterms", so we cannot map over those "subterms".
  -- Since the constructor does not change, the output term has the same sort as the input term (if mapping succeeds)
  mapSubterms _ = returnA ⊔ fail'

  cons = proc (c, ts) -> do
    (_,ctx) <- ask -< ()
    let (cParams,cSort) = signatures ctx M.! c
    if eqLength cParams ts && (Term (Tuple $ map sort ts) ctx)  ⊑ (Term (Tuple cParams)) ctx
      then returnA -< Term cSort ctx
      else returnA -< Term Top ctx

  numberLiteral = proc _ -> do
    (_,ctx) <- ask -< ()
    returnA -< Term Numerical ctx

  stringLiteral = proc _ -> do
    (_,ctx) <- ask -< ()
    returnA -< Term Lexical ctx

--instance Soundness StratEnv Interp where
--  sound senv xs f g = forAll (choose (0,3)) $ \i ->
--    let con :: A.Pow (Error () (TermEnv,_))
--        con = A.dedup $ alpha (fmap (\(x,tenv) -> C.runInterp f senv tenv x) xs)
--        abst :: A.Pow (Error () (TermEnv,_))
--        abst = A.dedup $ runInterp g i senv (alpha (fmap snd xs)) (alpha (fmap fst xs))
--    in counterexample (printf "%s ⊑/ %s" (show con) (show abst)) $ con ⊑ abst


instance Show Sort where
  show x = case x of
    Bottom -> "Bottom"
    Top -> "Top"
    Lexical -> "LEX"
    Numerical -> "NUM"
    Option s -> "Option (" ++ show s ++ ")"
    List s -> "List (" ++ show s ++ ")"
    Tuple ss -> "Tuple (" ++ intercalate ", " (map show ss) ++ ")"
    Sort (SortId s) -> unpack s

instance Hashable Sort



instance Show Term where
  show (Term s _) = show s

instance Hashable Term where
  hashWithSalt salt (Term s _) = salt `hashWithSalt` s

instance UpperBounded Term where
  top = Term Top (SortContext M.empty Set.empty Set.empty M.empty M.empty)

convertToList :: [Term] -> SortContext -> Term
convertToList [] ctx = Term (List Bottom) ctx
convertToList ts ctx = Term (List (sort $ lub ts)) ctx

instance PreOrd Term where
  Term Bottom _ ⊑ Term _ _ = True
  Term _ _ ⊑ Term Top _ = True

  Term s1 _ ⊑ Term s2 _ | s1 == s2 = True
  Term s1 ctx ⊑ Term s2 _ | Set.member s2 (injectionClosure ctx M.! s1) = True

  Term Lexical _ ⊑ t2 = isLexical t2
  Term (Option s1) ctx1 ⊑ Term (Option s2) ctx2 = Term s1 ctx1 ⊑ Term s2 ctx2
  Term (List s1) ctx1 ⊑ Term (List s2) ctx2 = Term s1 ctx1 ⊑ Term s2 ctx2
  Term (Tuple ss1) ctx1 ⊑ Term (Tuple ss2) ctx2 = (length ss1 == length ss2) &&
    (foldl (&&) True $ map (\(s1,s2) -> Term s1 ctx1 ⊑ Term s2 ctx2) $ zip ss1 ss2)

  _ ⊑ _ = False

instance Complete Term where
  t1 ⊔ t2 | t1 ⊑ t2 = t2
          | t2 ⊑ t1 = t1
          | otherwise = Term Top (context t1)

instance CoComplete Term where
  t1 ⊓ t2 | t1 ⊑ t2 = t1
          | t2 ⊑ t1 = t2
          | otherwise = Term Bottom (context t1)

--instance Galois (CP.Pow C.Term) Term where
--  alpha = lub . fmap go
--    where
--      go (C.Cons c ts) = Cons c (fmap go ts)
--      go (C.StringLiteral s) = StringLiteral s
--      go (C.NumberLiteral s) = NumberLiteral s
--  gamma = error "Infinite"
--
--instance Show Term where
--  show (Cons c ts) = show c ++ if null ts then "" else show ts
--  show (StringLiteral s) = show s
--  show (NumberLiteral n) = show n
--  show Wildcard = "_"
--
--instance Num Term where
--  t1 + t2 = Cons "Add" [t1,t2]
--  t1 - t2 = Cons "Sub" [t1,t2]
--  t1 * t2 = Cons "Mul" [t1,t2]
--  abs t = Cons "Abs" [t]
--  signum t = Cons "Signum" [t]
--  fromInteger = NumberLiteral . fromIntegral
--
--instance Hashable Term where
--  hashWithSalt s (Cons c ts) = s `hashWithSalt` (0::Int) `hashWithSalt` c `hashWithSalt` ts
--  hashWithSalt s (StringLiteral t) = s `hashWithSalt` (1::Int) `hashWithSalt` t
--  hashWithSalt s (NumberLiteral n) = s `hashWithSalt` (2::Int) `hashWithSalt` n
--  hashWithSalt s Wildcard = s `hashWithSalt` (3::Int)
--
--instance NFData Term where
--  rnf t = case t of
--    Cons c ts -> rnf c `seq` rnf ts
--    StringLiteral s -> rnf s
--    NumberLiteral n -> rnf n
--    Wildcard -> ()
--
--instance Arbitrary Term where
--  arbitrary = do
--    he <- choose (0,7)
--    wi <- choose (0,4)
--    arbitraryTerm he wi
--
--arbitraryTerm :: Int -> Int -> Gen Term
--arbitraryTerm 0 _ =
--  oneof
--    [ Cons <$> arbitrary <*> pure []
--    , StringLiteral <$> arbitraryLetter
--    , NumberLiteral <$> choose (0,9)
--    , pure Wildcard
--    ]
--arbitraryTerm h w = do
--  w' <- choose (0,w)
--  c <- arbitrary
--  fmap (Cons c) $ vectorOf w' $ join $
--    arbitraryTerm <$> choose (0,h-1) <*> pure w
--
--internal :: Arrow c => c (HashMap TermVar Term) (HashMap TermVar Term) -> c TermEnv TermEnv
--internal f = arr TermEnv . f . arr (\(TermEnv e) -> e)
--
--map :: ArrowChoice c => c Term Term -> c TermEnv TermEnv
--map f = internal (arr M.fromList . mapA (second f) . arr M.toList)

dom :: HashMap TermVar t -> [TermVar]
dom = M.keys

instance PreOrd TermEnv where
  TermEnv env1 ⊑ TermEnv env2 =
    Prelude.all (\v -> fromMaybe (M.lookup v env1) ⊑ fromMaybe (M.lookup v env2)) (dom env2)

instance Complete TermEnv where
  TermEnv env1' ⊔ TermEnv env2' = go (dom env1') env1' env2' M.empty
    where
      go vars env1 env2 env3 = case vars of
        (v:vs) -> case (M.lookup v env1,M.lookup v env2) of
          (Just t1,Just t2) -> go vs env1 env2 (M.insert v (t1⊔t2) env3)
          _                 -> go vs env1 env2 env3
        [] -> TermEnv env3

instance UpperBounded TermEnv where
  top = TermEnv M.empty

--instance Galois (CP.Pow C.TermEnv) TermEnv where
--  alpha = lub . fmap (\(C.TermEnv e) -> TermEnv (fmap alphaSing e))
--  gamma = undefined

-- prim :: (ArrowTry p, ArrowAppend p, IsTerm t p, IsTermEnv (AbstractTermEnv t) t p)
--      => StratVar -> [TermVar] -> p a t
-- prim f ps = undefined
  -- proc _ -> case f of
  --   "SSL_strcat" -> do
  --     args <- lookupTermArgs -< ps
  --     case args of
  --       [T.StringLiteral t1, T.StringLiteral t2] -> stringLiteral -< t1 `append` t2
  --       [T.Wildcard, _] -> wildcard -< ()
  --       [_, T.Wildcard] -> wildcard -< ()
  --       _ -> fail -< ()
  --   "SSL_newname" -> do
  --     args <- lookupTermArgs -< ps
  --     case args of
  --       [T.StringLiteral _] -> wildcard -< ()
  --       [T.Wildcard] -> wildcard -< ()
  --       _ -> fail -< ()
  --   _ -> error ("unrecognized primitive function: " ++ show f) -< ()
  -- where
  --   lookupTermArgs = undefined
      -- proc args -> do
      -- tenv <- getTermEnv -< ()
      -- case mapM (`M.lookup` tenv) args of
      --   Just t -> mapA matchTerm -< t
      --   Nothing -> fail <+> success -< [T.Wildcard | _ <- args]
-- {-# SPECIALISE prim :: StratVar -> [TermVar] -> Interp StratEnv TermEnv PowersetResult Term Term #-}
 
