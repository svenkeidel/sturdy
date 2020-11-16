{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Syntax where

import           Prelude hiding (id)
import           SortContext (Context,Sort,Signature)
import qualified SortContext as I

import           Control.Monad.Except
import           Control.Monad.Writer (Writer, tell, runWriter)
import           Control.Monad.State
import           Control.DeepSeq

import           Data.ATerm
import           Data.Constructor
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.Hashable
import           Data.List (intercalate)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.String (IsString(..))
import           Data.Text (Text,pack,unpack)
import qualified Data.Text as T
import           Data.Lens(Prism')
import qualified Data.Lens as L
import           Data.Label
import           Data.Empty
import           Data.Order

import           GHC.Generics(Generic)
import           GHC.Exts(IsList(..))

import           Text.Read (readMaybe)
import           Test.QuickCheck (Arbitrary(..),Gen)
import qualified Test.QuickCheck as Q

-- | Expressions for the Stratego core language are called strategies.
data Strat
  = Fail Label
  | Id Label
  | Seq Strat Strat Label
  | GuardedChoice Strat Strat Strat Label
  | One Strat Label
  | Some Strat Label
  | All Strat Label
  | Match TermPattern Label
  | Build TermPattern Label
  | Scope [TermVar] Strat Label
  | Let [(StratVar,Strategy)] Strat Label
  | Call StratVar [Strat] [TermVar] Label
  | Prim StratVar [Strat] [TermVar] Label
  | Apply Strat Label
  deriving stock (Generic,Eq)
  deriving PreOrd via Discrete Strat
  deriving anyclass NFData

type LStrat = State Label Strat

fail :: MonadState Label m => m Strat
fail = Fail <$> fresh

id :: MonadState Label m => m Strat
id = Id <$> fresh

seq :: MonadState Label m => m Strat -> m Strat -> m Strat
seq e1 e2 = Seq <$> e1 <*> e2 <*> fresh

guardedChoice :: MonadState Label m => m Strat -> m Strat -> m Strat -> m Strat
guardedChoice e1 e2 e3 = GuardedChoice <$> e1 <*> e2 <*> e3 <*> fresh

leftChoice :: MonadState Label m => m Strat -> m Strat -> m Strat
leftChoice f = guardedChoice f id

one :: MonadState Label m => m Strat -> m Strat
one e = One <$> e <*> fresh

some :: MonadState Label m => m Strat -> m Strat
some e = Some <$> e <*> fresh

all :: MonadState Label m => m Strat -> m Strat
all e = All <$> e <*> fresh

match :: MonadState Label m => TermPattern -> m Strat
match pat = Match pat <$> fresh

build :: MonadState Label m => TermPattern -> m Strat
build pat = Build pat <$> fresh

scope :: MonadState Label m => [TermVar] -> m Strat -> m Strat
scope vars body = Scope vars <$> body <*> fresh

let_ :: MonadState Label m => [(StratVar,m Strategy)] -> m Strat -> m Strat
let_ binds body = Let <$> sequence [ (var,) <$> strat | (var,strat) <- binds ] <*> body <*> fresh

call :: MonadState Label m => StratVar -> [m Strat] -> [TermVar] -> m Strat
call sv ss tvs = Call <$> pure sv <*> sequence ss <*> pure tvs <*> fresh

prim :: MonadState Label m => StratVar -> [m Strat] -> [TermVar] -> m Strat
prim sv ss tvs = Prim <$> pure sv <*> sequence ss <*> pure tvs <*> fresh

-- | Pattern to match and build terms.
data TermPattern
  = As TermVar TermPattern
  | Cons Constructor [TermPattern]
  | Explode TermPattern TermPattern
  | Var TermVar
  | StringLiteral Text
  | NumberLiteral Int
  deriving stock (Eq,Generic)
  deriving anyclass (NFData,Hashable)

-- | Stratego source code is organized in modules consisting of a
-- signature describing possible shapes of terms and named strategies.
data Module = Module Context Strategies
  deriving stock (Show,Eq)

type Strategies = HashMap StratVar Strategy

-- | A named strategy takes two kinds of arguments, arguments that
-- refer to other named strategies and arguments that refer to terms.
-- Additionally, a strategy takes and input term and eventually
-- produces an output term.
data Strategy = Strategy { strategyStratArguments :: [StratVar]
                         , strategyTermArguments :: [TermVar]
                         , strategyBody :: Strat
                         , strategyLabel :: Label
                         }
  deriving stock (Show,Generic)
  deriving anyclass (NFData)

instance HasLabel Strategy where
  label = strategyLabel

instance Eq Strategy where
  s1 == s2 = label s1 == label s2

strategy :: MonadState Label m => [StratVar] -> [TermVar] -> m Strat -> m Strategy
strategy svs tvs body = Strategy svs tvs <$> body <*> fresh

-- data Closure = Closure Strategy StratEnv
--   deriving stock (Eq,Generic)
--   deriving PreOrd via Discrete Closure
--   deriving anyclass (NFData,Hashable)

type StratEnv = HashMap StratVar Strategy
type LStratEnv = State Label StratEnv

instance IsEmpty LStratEnv where
  empty = return empty

stratEnv :: MonadState Label m => [(StratVar,m Strategy)] -> m StratEnv
stratEnv l = M.fromList <$> sequence [ (sv,) <$> strat | (sv,strat) <- l ]

filterStratEnv :: HashSet StratVar -> StratEnv -> StratEnv
filterStratEnv vars = M.filterWithKey (\var _ -> var `H.member` vars)

newtype TermVar = TermVar Text   deriving newtype (Eq,Ord,Hashable,NFData)
newtype StratVar = StratVar Text deriving newtype (Eq,Ord,IsString,Hashable,NFData)

liftStrategyScopes :: Strategy -> Strategy
liftStrategyScopes (Strategy svs tvs s l) = Strategy svs tvs (liftStratScopes s l) l

liftStratScopes :: Strat -> Label -> Strat
liftStratScopes s l =
  let (s', vs) = runWriter (liftScopes s) in
  if length vs == S.size (S.fromList vs)
    then if null vs
      then s'
      else Scope vs s' l
    else error $ "found duplicate scope vars " ++ show vs

liftScopes :: Strat -> Writer [TermVar] Strat
liftScopes strat = case strat of
  Id l -> return (Id l)
  Fail l -> return (Fail l)
  Seq s1 s2 l -> do
    s1' <- liftScopes s1
    s2' <- liftScopes s2
    return $ Seq s1' s2' l
  GuardedChoice s1 s2 s3 l -> do
    s1' <- liftScopes s1
    s2' <- liftScopes s2
    s3' <- liftScopes s3
    return $ GuardedChoice s1' s2' s3' l
  One s l -> One <$> liftScopes s <*> pure l
  Some s l -> Some <$> liftScopes s <*> pure l
  All s l -> All <$> liftScopes s <*> pure l
  Scope xs s _ -> do
    tell xs
    liftScopes s
  Match f l -> return $ Match f l
  Build f l -> return $ Build f l
  Let bnds body l -> do
    let bnds' = map (\(v,s) -> (v,liftStrategyScopes s)) bnds
    body' <- liftScopes body
    return $ Let bnds' body' l
  Call f ss ts l -> do
    let ss' = map (\s -> liftStratScopes s l) ss
    return $ Call f ss' ts l
  Prim f ss ts l -> do
    let ss' = map (\s -> liftStratScopes s l) ss
    return $ Prim f ss' ts l
  Apply body l -> Apply <$> liftScopes body <*> pure l

-- instance Monoid TermVarSet where
--   mempty = TermVarSet mempty
--   mappend (TermVarSet s1) (TermVarSet s2) =
--     if S.null (S.intersection s1 s2)
--       then TermVarSet $ S.union s1 s2
--       else error $ "non-unique scope vars " ++ show (s1,s2)

moduleStratEnv :: Module -> StratEnv
moduleStratEnv (Module _ senv) = M.fromList [ (var,strat) | (var,strat) <- M.toList senv ]

signature :: Module -> Context
signature (Module sig _) = sig

linear :: TermPattern -> Bool
linear p = S.size (termVars p :: Set TermVar) == length (termVars p :: [TermVar])

parseModule :: (MonadError String m, MonadState Label m) => ATerm -> m Module
parseModule t = case t of
  ATerm "Specification" [List [sig, ATerm "Strategies" [List strats]]] ->
    Module <$> parseSignature sig <*> (M.fromList <$> traverse parseStrategy strats)
  _ -> throwError $ "unexpected input while parsing module from aterm: " ++ show t

parseSignature :: MonadError String m => ATerm -> m Context
parseSignature s = case s of
  ATerm "Signature" [List sig] -> parseConstructors sig I.empty
  _ -> throwError $ "unexpected input while parsing signature from aterm: " ++ show s
  where
    parseConstructors :: MonadError String m => [ATerm] -> Context -> m Context
    parseConstructors ts sig = case ts of
      ATerm "Constructors" [List constrs]:rest -> do
        sig' <- foldM parseDeclaration sig constrs
        parseConstructors rest sig'
      [] -> return sig
      _ -> throwError $ "unexpected input while parsing signature from aterm: " ++ show ts

parseDeclaration :: MonadError String m => Context -> ATerm -> m Context
parseDeclaration sig t
  | containsSortVar t = return sig
  | otherwise = case t of
      ATerm "OpDecl" [String con, body] -> do
        typ <- parseFun body
        return $ I.insertSignature (Constructor con) typ sig
      ATerm "OpDeclInj" [f@(ATerm "FunType" [_, ATerm "ConstType" [ATerm "SortTuple" _]])] ->
        parseDeclaration sig $ ATerm "OpDecl" [String "", f]
      ATerm "OpDeclInj" [ATerm "FunType" [List[ty1], ty2]] ->
        I.insertSubtype <$> parseSort ty1 <*> parseSort ty2 <*> pure sig
      ATerm "OpDeclInj" [ATerm "ConstType" _] ->
        return sig
      _ -> throwError $ "unexpected input while parsing declaration from aterm: " ++ show t  
  where
    containsSortVar :: ATerm -> Bool
    containsSortVar (ATerm "SortVar" _) = True
    containsSortVar (ATerm _ ts) = any containsSortVar ts
    containsSortVar (List ts) = any containsSortVar ts
    containsSortVar _ = False

parseFun :: MonadError String m => ATerm -> m Signature
parseFun t = case t of
  ATerm "ConstType" [res] -> I.Signature [] <$> parseSort res
  ATerm "FunType" [List args,res] -> I.Signature <$> traverse parseSort args <*> parseSort res
  _ -> throwError $ "unexpected input while parsing function type from aterm: " ++ show t

parseSort :: MonadError String m => ATerm -> m Sort
parseSort t = case t of
  ATerm "ConstType" [res] -> parseSort res
  ATerm "SortNoArgs" [String "String"] -> return I.Lexical
  ATerm "SortNoArgs" [String "Int"] -> return I.Numerical
  ATerm "SortNoArgs" [String sortName] -> return $ I.Sort (I.SortId sortName)
  ATerm "Sort" [String "String", List []] -> return I.Lexical
  ATerm "Sort" [String "Int", List []] -> return I.Numerical
  ATerm "Sort" [String sortName, List []] -> return $ I.Sort (I.SortId sortName)
  ATerm "Sort" [String "List", List [s]] -> I.List <$> parseSort s
  ATerm "Sort" [String "List", List ss] -> I.List . I.Tuple <$> traverse parseSort ss
  ATerm "Sort" [String "Option", List [s]] -> I.Option <$> parseSort s
  ATerm "Sort" [String "Option", List ss] -> I.Option . I.Tuple <$> traverse parseSort ss
  ATerm "SortTuple" [List args] -> I.Tuple <$> traverse parseSort args
  _ -> throwError $ "unexpected input while parsing sort from aterm: " ++ show t

parseStrategy :: (MonadError String m, MonadState Label m) => ATerm -> m (StratVar,Strategy)
parseStrategy strat = case strat of
  ATerm "SDefT" [String name, List stratVars, List ts, body] -> do
    svs <- parseStratVars stratVars
    tvs <- parseTermVars ts
    str <- strategy svs tvs (parseStrat body)
    return (StratVar name, liftStrategyScopes str)
  ATerm "ExtSDef" [String name, List ts, List stratVars] -> do
    svs <- parseStratVars stratVars
    tvs <- parseTermVars ts
    str <- strategy svs tvs id
    return (StratVar name, liftStrategyScopes str)
  _ -> throwError $ "unexpected input while parsing strategy from aterm: " ++ show strat

parseStratVars :: MonadError String m => [ATerm] -> m [StratVar]
parseStratVars vars =
  forM vars $ \var -> case var of
    ATerm "VarDec" [String name, _] -> return $ StratVar name
    ATerm _ [String x] -> return $ StratVar x
    _ -> throwError $ "unexpected input while parsing strategy variables from aterm: " ++ show var

parseTermVar :: MonadError String m => ATerm -> m TermVar
parseTermVar var = case var of 
  ATerm "VarDec" [String name, _] -> return $ TermVar name
  ATerm "Var" [String name] -> return $ TermVar name
  ATerm "Wld" [] -> return $ TermVar "_"
  ATerm "Str" [String ""] -> return $ TermVar "_"
  String name -> return $ TermVar name
  _ -> throwError $ "unexpected input while parsing term variables from aterm: " ++ show var

parseTermVars :: MonadError String m => [ATerm] -> m [TermVar]
parseTermVars = traverse parseTermVar

parseStrat :: (MonadError String m, MonadState Label m) => ATerm -> m Strat
parseStrat t = case t of
  ATerm "Id" [] -> Id <$> fresh
  ATerm "Fail" [] -> Fail <$> fresh
  ATerm "Seq" [e1, e2] -> Seq <$> parseStrat e1 <*> parseStrat e2 <*> fresh
  ATerm "GuardedLChoice" [e1, e2, e3] -> GuardedChoice <$> parseStrat e1 <*> parseStrat e2 <*> parseStrat e3 <*> fresh
  ATerm "All" [e] -> All <$> parseStrat e <*> fresh
  ATerm "Some" [e] -> Some <$> parseStrat e <*> fresh
  ATerm "One" [e] -> One <$> parseStrat e <*> fresh
  ATerm "Match" [tp] -> Match <$> parseTermPattern tp <*> fresh
  ATerm "Build" [tp] -> Build <$> parseTermPattern tp <*> fresh
  ATerm "Scope" [List vars, e] -> Scope <$> parseTermVars vars <*> parseStrat e <*> fresh
  ATerm "Let" [List strats, body] ->
    Let <$> traverse parseStrategy strats <*> parseStrat body <*> fresh
  ATerm "CallT" [ATerm "SVar" [String svar], List stratArgs, List termArgs] ->
    Call (StratVar svar) <$> traverse parseStrat stratArgs <*> parseTermVars termArgs <*> fresh
  ATerm "PrimT" [String svar, List stratArgs, List termArgs] ->
    Prim (StratVar svar) <$> traverse parseStrat stratArgs <*> parseTermVars termArgs <*> fresh
  _ -> throwError $ "unexpected input while parsing strategy from aterm: " ++ show t

parseTermPattern :: MonadError String m => ATerm -> m TermPattern
parseTermPattern p = case p of
  ATerm "Anno" [p1,_] -> parseTermPattern p1 -- Ignore annotations
  ATerm "As" [v@(ATerm "Var" _), p1] -> As <$> parseTermVar v <*> parseTermPattern p1
  ATerm "Op" [String con, List subterms] ->
    Cons (Constructor con) <$> traverse parseTermPattern subterms
  ATerm "Str" [String str] -> return $ StringLiteral str
  ATerm "Var" _ -> Var <$> parseTermVar p
  ATerm "Wld" _ -> Var <$> parseTermVar p
  ATerm "Explode" [p1,p2] -> Explode <$> parseTermPattern p1 <*> parseTermPattern p2
  ATerm "Int" [i] -> NumberLiteral <$> parseInt i
  _ -> throwError $ "unexpected input while parsing term pattern from aterm: " ++ show p


parseInt :: MonadError String m => ATerm -> m Int
parseInt s = case s of
  String t -> case readMaybe (T.unpack t) of
    Just i -> return i
    Nothing -> throwError $ "could not parse integer literal from text: " ++ show t
  _ -> throwError $ "unexpected input while parsing integer literal from aterm: " ++ show s

instance Arbitrary TermVar where
  arbitrary = TermVar . T.singleton <$> Q.choose ('a','z')

instance Show TermVar where
  show (TermVar x) = unpack x

instance IsString TermVar where
  fromString = TermVar . pack

instance IsString TermPattern where
  fromString = Var . fromString

instance Show TermPattern where
  show (As v t) = show v ++ "@(" ++ show t ++ ")"
  show (Cons c ts) = show c ++ if null ts then "" else show ts
  show (Var x) = show x
  show (Explode f xs) = show f ++ "#(" ++ show xs ++ ")"
  show (StringLiteral s) = show s
  show (NumberLiteral n) = show n

-- instance IsString Strat where
--   fromString s = Call (fromString s) [] [] 

instance HasLabel Strat where
  label x = case x of
    Fail l -> l
    Id l -> l
    Seq _ _ l -> l
    GuardedChoice _ _ _ l -> l
    One _ l -> l
    Some _ l -> l
    All _ l -> l
    Match _ l -> l
    Build _ l -> l
    Scope _ _ l -> l
    Call _ _ _ l -> l
    Let _ _ l -> l
    Prim _ _ _ l -> l
    Apply _ l -> l

instance Hashable Strat where
  hashWithSalt s x = s `hashWithSalt` label x

instance Show Strat where
  showsPrec d s0 = case s0 of
    Fail _ ->
      showString "fail"
    Id _ ->
      showString "id"
    Seq s1 s2 _ ->
      showParen (d > seq_prec)
        $ showsPrec seq_prec s1
        . showString "; "
        . showsPrec seq_prec s2
    GuardedChoice s1 s2 s3 _ ->
      showParen (d > choice_prec)
        $ showsPrec (choice_prec+1) s1
        . showString " < "
        . showsPrec (choice_prec+1) s2
        . showString " + "
        . showsPrec (choice_prec+1) s3
    One s _ ->
      showParen (d > app_prec)
        $ showString "one "
        . showsPrec (app_prec+1) s
    Some s _ ->
      showParen (d > app_prec)
        $ showString "some "
        . showsPrec (app_prec+1) s
    All s _ ->
      showParen (d > app_prec)
        $ showString "all "
        . showsPrec (app_prec+1) s
    Match t _ ->
      showString "?"
        . showsPrec (app_prec+1) t
    Build t _ ->
      showString "!"
        . showsPrec (app_prec+1) t
    Scope vars s _ ->
      showString "{ "
      . showString (intercalate "," (map show vars))
      . showString ": "
      . shows s
      . showString " }"
    Call (StratVar f) ss ts _ ->
     showString (unpack f)
     . showString "("
     . showString (intercalate "," (map show ss))
     . showString "|"
     . showString (intercalate "," (map show ts))
     . showString ")"
    Apply body _ ->
      showsPrec d body
    Prim (StratVar f) ss ts _ ->
     showString (unpack f)
     . showString "("
     . showString (intercalate "," (map show ss))
     . showString "|"
     . showString (intercalate "," (map show ts))
     . showString ")"
    Let ss body _ ->
     showString "let "
     . showString (intercalate "," (map show ss))
     . showString " in "
     . shows body
     . showString " end"
    where
      app_prec = 10
      seq_prec = 9
      choice_prec = 8

instance Hashable Strategy where
  hashWithSalt s strat = s `hashWithSalt` strategyLabel strat

instance Show StratVar where
  show (StratVar x) = unpack x

instance Num TermPattern where
  t1 + t2 = Cons "Add" [t1,t2]
  t1 - t2 = Cons "Sub" [t1,t2]
  t1 * t2 = Cons "Mul" [t1,t2]
  abs t = Cons "Abs" [t]
  signum t = Cons "Signum" [t]
  fromInteger = NumberLiteral . fromIntegral

instance Arbitrary TermPattern where
  arbitrary = do
    h <- Q.choose (0,7)
    w <- Q.choose (0,4)
    arbitraryTermPattern h w Q.arbitrary

arbitraryTermPattern :: Int -> Int -> Gen TermVar -> Gen TermPattern
arbitraryTermPattern h w var
  | h == 0 =
      Q.oneof
        [ Cons <$> Q.arbitrary <*> pure []
        , StringLiteral <$> arbitraryLetter
        , NumberLiteral <$> Q.choose (0,9)
        , Var <$> var
        ]
  | otherwise = do
      w' <- Q.choose (0,w)
      Q.oneof
        [ do
          c <- Q.arbitrary
          fmap (Cons c) $ Q.vectorOf w' $ do
            h' <- Q.choose (0,h-1)
            arbitraryTermPattern h' w var
        -- , Explode <$> arbitraryStringPattern
        --           <*> arbitraryListPattern w'
        ]

  -- where
  --   arbitraryStringPattern :: Gen TermPattern
  --   arbitraryStringPattern =
  --     oneof
  --       [ Var <$> var
  --       , StringLiteral <$> arbitraryLetter
  --       ]

  --   arbitraryListPattern :: Int -> Gen TermPattern
  --   arbitraryListPattern len =
  --     if len == 0
  --       then return $ Cons "Nil" []
  --       else do
  --         h' <- choose (0,h-1)
  --         tp <- arbitraryTermPattern h' w var
  --         tl <- arbitraryListPattern (len-1)
  --         return $ Cons "Cons" [tp,tl]

class TermVars s where
  termVars :: (IsList (f TermVar), Item (f TermVar) ~ TermVar, Monoid (f TermVar)) => s -> f TermVar

instance TermVars Module where
  termVars (Module _ strats) = termVars strats

instance (TermVars x,TermVars y) => TermVars (HashMap x y) where
  termVars = termVars . M.toList

-- instance TermVars env => TermVars (Closure Strategy env) where
--   termVars (Closure s env) = termVars s <> termVars env

instance TermVars Strat where
  termVars s = case s of
    Fail _ -> mempty
    Id _ -> mempty
    Seq s1 s2 _ -> termVars s1 <> termVars s2
    GuardedChoice s1 s2 s3 _ -> termVars s1 <> termVars s2 <> termVars s3
    One s1 _ -> termVars s1
    Some s1 _ -> termVars s1
    All s1 _ -> termVars s1
    Match p _ -> termVars p
    Build p _ -> termVars p
    Scope vs s1 _ -> termVars vs <> termVars s1
    Let strats s1 _ -> termVars strats <> termVars s1
    Call _ _ vs _ -> termVars vs
    Prim _ _ vs _ -> termVars vs
    Apply body _ -> termVars body

instance TermVars StratVar where
  termVars _ = mempty

instance TermVars Strategy where
  termVars strat = termVars (strategyTermArguments strat) <> termVars (strategyBody strat)

instance TermVars TermPattern where
  termVars t = case t of
    As v p -> termVars v <> termVars p
    Cons _ ts -> termVars ts
    Explode f l -> termVars f <> termVars l
    Var x -> termVars x
    _ -> mempty

instance TermVars x => TermVars [x] where
  termVars l = mconcat $ termVars <$> l

instance (TermVars x, TermVars y) => TermVars (x,y) where
  termVars (x,y) = termVars x <> termVars y

instance TermVars TermVar where
  termVars "_" = mempty
  termVars x = fromList [x]

stratCall :: Prism' (tenv,(senv,(Strat,term))) ((Strat,senv),(term,tenv))
stratCall = L.prism' (\((strat,senv),(term,tenv)) -> (tenv,(senv,(strat,term))))
                (\(tenv,(senv,(strat,term))) -> case strat of
                   Call {} -> Just ((strat,senv),(term,tenv))
                   _ -> Nothing)
{-# INLINE stratCall #-}

stratApply :: (Hashable tenv, Hashable senv, Hashable term) => Prism' (tenv,(senv,(Strat,term))) ((Hashed Strat, Hashed senv),(Hashed term,Hashed tenv))
stratApply = L.prism' (\((strat,senv),(term,tenv)) -> (unhashed tenv,(unhashed senv,(unhashed strat,unhashed term))))
                (\(tenv,(senv,(strat,term))) -> case strat of
                   Apply {} -> Just ((hashed strat,hashed senv),(hashed term,hashed tenv))
                   _ -> Nothing)
{-# INLINE stratApply #-}
