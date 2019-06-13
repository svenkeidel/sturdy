{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Syntax where

import           SortContext (Context,Sort,Signature)
import qualified SortContext as I

import           Control.Monad.Except
import           Control.Monad.Writer (Writer, tell, runWriter)
import           Control.DeepSeq

import           Data.ATerm
import           Data.Constructor
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Hashable
import           Data.List (intercalate)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.String (IsString(..))
import           Data.Text (Text,pack,unpack)
import qualified Data.Text as T
import           Data.Lens(Prism')
import qualified Data.Lens as L

import           GHC.Generics(Generic)
import           GHC.Exts(IsList(..))

import           Text.Read (readMaybe)
import           Test.QuickCheck (Arbitrary(..),Gen)
import qualified Test.QuickCheck as Q

-- | Expressions for the Stratego core language are called strategies.
data Strat
  = Fail
  | Id
  | Seq Strat Strat
  | GuardedChoice Strat Strat Strat
  | One Strat
  | Some Strat
  | All Strat
  | Match TermPattern
  | Build TermPattern
  | Scope [TermVar] Strat
  | Let [(StratVar,Strategy)] Strat
  | Call StratVar [Strat] [TermVar]
  | Prim StratVar [Strat] [TermVar]
  | Apply Strat
  deriving (Eq,Generic)
instance NFData Strat

-- | Pattern to match and build terms.
data TermPattern
  = As TermVar TermPattern
  | Cons Constructor [TermPattern]
  | Explode TermPattern TermPattern
  | Var TermVar
  | StringLiteral Text
  | NumberLiteral Int
  deriving (Eq,Generic)
instance NFData TermPattern

-- | Stratego source code is organized in modules consisting of a
-- signature describing possible shapes of terms and named strategies.
data Module = Module Context Strategies deriving (Show,Eq)

type Strategies = HashMap StratVar Strategy

-- | A named strategy takes two kinds of arguments, arguments that
-- refer to other named strategies and arguments that refer to terms.
-- Additionally, a strategy takes and input term and eventually
-- produces an output term.
data Strategy = Strategy [StratVar] [TermVar] Strat deriving (Show,Eq,Generic)
instance NFData Strategy

data Closure = Closure Strategy StratEnv deriving (Eq,Generic)
instance NFData Closure

instance Hashable Closure where
  hashWithSalt s (Closure strat senv) = s `hashWithSalt` strat `hashWithSalt` senv

type StratEnv = HashMap StratVar Closure
newtype TermVar = TermVar Text deriving (Eq,Ord,Hashable,NFData)
newtype StratVar = StratVar Text deriving (Eq,Ord,IsString,Hashable,NFData)

liftStrategyScopes :: Strategy -> Strategy
liftStrategyScopes (Strategy svs tvs s) = Strategy svs tvs (liftStratScopes s)

liftStratScopes :: Strat -> Strat
liftStratScopes s =
  let (s', vs) = runWriter (liftScopes s) in
  if length vs == S.size (S.fromList vs)
    then if null vs
      then s'
      else Scope vs s'
    else error $ "found duplicate scope vars " ++ show vs

liftScopes :: Strat -> Writer [TermVar] Strat
liftScopes strat = case strat of
    Id -> return Id
    Fail -> return Fail
    Seq s1 s2 -> do
      s1' <- liftScopes s1
      s2' <- liftScopes s2
      return $ Seq s1' s2'
    GuardedChoice s1 s2 s3 -> do
      s1' <- liftScopes s1
      s2' <- liftScopes s2
      s3' <- liftScopes s3
      return $ GuardedChoice s1' s2' s3'
    One s -> One <$> liftScopes s
    Some s -> Some <$> liftScopes s
    All s -> All <$> liftScopes s
    Scope xs s -> do
      tell xs
      liftScopes s
    Match f -> return $ Match f
    Build f -> return $ Build f
    Let bnds body -> do
      let bnds' = map (\(v,s) -> (v,liftStrategyScopes s)) bnds
      body' <- liftScopes body
      return $ Let bnds' body'
    Call f ss ts -> do
      let ss' = map liftStratScopes ss
      return $ Call f ss' ts
    Prim f ss ts -> do
      let ss' = map liftStratScopes ss
      return $ Prim f ss' ts
    Apply body -> Apply <$> liftScopes body

-- instance Monoid TermVarSet where
--   mempty = TermVarSet mempty
--   mappend (TermVarSet s1) (TermVarSet s2) =
--     if S.null (S.intersection s1 s2)
--       then TermVarSet $ S.union s1 s2
--       else error $ "non-unique scope vars " ++ show (s1,s2)


leftChoice :: Strat -> Strat -> Strat
leftChoice f = GuardedChoice f Id

stratEnv :: Module -> StratEnv
stratEnv (Module _ senv) = fmap (`Closure` M.empty) senv

signature :: Module -> Context
signature (Module sig _) = sig

linear :: TermPattern -> Bool
linear p = S.size (termVars p :: Set TermVar) == length (termVars p :: [TermVar])

parseModule :: MonadError String m => ATerm -> m Module
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
  ATerm "SortNoArgs" [String "String"] -> return $ I.Lexical
  ATerm "SortNoArgs" [String "Int"] -> return $ I.Numerical
  ATerm "SortNoArgs" [String sortName] -> return $ I.Sort (I.SortId sortName)
  ATerm "Sort" [String "String", List []] -> return $ I.Lexical
  ATerm "Sort" [String "Int", List []] -> return $ I.Numerical
  ATerm "Sort" [String sortName, List []] -> return $ I.Sort (I.SortId sortName)
  ATerm "Sort" [String "List", List [s]] -> I.List <$> parseSort s
  ATerm "Sort" [String "List", List ss] -> I.List <$> I.Tuple <$> traverse parseSort ss
  ATerm "Sort" [String "Option", List [s]] -> I.Option <$> parseSort s
  ATerm "Sort" [String "Option", List ss] -> I.Option <$> I.Tuple <$> traverse parseSort ss
  ATerm "SortTuple" [List args] -> I.Tuple <$> traverse parseSort args
  _ -> throwError $ "unexpected input while parsing sort from aterm: " ++ show t

parseStrategy :: MonadError String m => ATerm -> m (StratVar,Strategy)
parseStrategy strat = case strat of
  ATerm "SDefT" [String name, List stratVars, List ts, body] -> do
    str <- Strategy <$> parseStratVars stratVars
                    <*> parseTermVars ts
                    <*> parseStrat body
    return (StratVar name, liftStrategyScopes str)
  ATerm "ExtSDef" [String name, List ts, List stratVars] -> do
    str <- Strategy <$> parseStratVars stratVars
                    <*> parseTermVars ts
                    <*> return Id
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

parseStrat :: MonadError String m => ATerm -> m Strat
parseStrat t = case t of
  ATerm "Id" [] -> return Id
  ATerm "Fail" [] -> return Fail
  ATerm "Seq" [e1, e2] -> Seq <$> parseStrat e1 <*> parseStrat e2
  ATerm "GuardedLChoice" [e1, e2, e3] -> GuardedChoice <$> parseStrat e1 <*> parseStrat e2 <*> parseStrat e3
  ATerm "All" [e] -> All <$> parseStrat e
  ATerm "Some" [e] -> Some <$> parseStrat e
  ATerm "One" [e] -> One <$> parseStrat e
  ATerm "Match" [tp] -> Match <$> parseTermPattern tp
  ATerm "Build" [tp] -> Build <$> parseTermPattern tp
  ATerm "Scope" [List vars, e] -> Scope <$> parseTermVars vars <*> parseStrat e
  ATerm "Let" [List strats, body] ->
    Let <$> traverse parseStrategy strats <*> parseStrat body
  ATerm "CallT" [ATerm "SVar" [String svar], List stratArgs, List termArgs] ->
    Call (StratVar svar) <$> traverse parseStrat stratArgs <*> parseTermVars termArgs
  ATerm "PrimT" [String svar, List stratArgs, List termArgs] ->
    Prim (StratVar svar) <$> traverse parseStrat stratArgs <*> parseTermVars termArgs
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

instance Hashable TermPattern where
  hashWithSalt s x = case x of
    As v t -> s `hashWithSalt` (0::Int) `hashWithSalt` v `hashWithSalt` t
    Cons c ts -> s `hashWithSalt` (1::Int) `hashWithSalt` c `hashWithSalt` ts
    Explode t1 t2 -> s `hashWithSalt` (2::Int) `hashWithSalt` t1 `hashWithSalt` t2
    Var tv -> s `hashWithSalt` (3::Int) `hashWithSalt` tv
    StringLiteral l -> s `hashWithSalt` (4::Int) `hashWithSalt` l
    NumberLiteral l -> s `hashWithSalt` (5::Int) `hashWithSalt` l

instance Show TermPattern where
  show (As v t) = show v ++ "@(" ++ show t ++ ")"
  show (Cons c ts) = show c ++ if null ts then "" else show ts
  show (Var x) = show x
  show (Explode f xs) = show f ++ "#(" ++ show xs ++ ")"
  show (StringLiteral s) = show s
  show (NumberLiteral n) = show n

instance IsString Strat where
  fromString s = Call (fromString s) [] []

instance Hashable Strat where
  hashWithSalt s x = case x of
    Fail -> s `hashWithSalt` (0::Int)
    Id -> s `hashWithSalt` (1::Int)
    Seq e1 e2 -> s `hashWithSalt` (2::Int) `hashWithSalt` e1 `hashWithSalt` e2
    GuardedChoice e1 e2 e3 -> s `hashWithSalt` (3::Int) `hashWithSalt` e1 `hashWithSalt` e2 `hashWithSalt` e3
    One e -> s `hashWithSalt` (4::Int) `hashWithSalt` e
    Some e -> s `hashWithSalt` (5::Int) `hashWithSalt` e
    All e -> s `hashWithSalt` (6::Int) `hashWithSalt` e
    Match t -> s `hashWithSalt` (7::Int) `hashWithSalt` t
    Build t -> s `hashWithSalt` (8::Int) `hashWithSalt` t
    Scope tv e -> s `hashWithSalt` (9::Int) `hashWithSalt` tv `hashWithSalt` e
    Call sv ss tv -> s `hashWithSalt` (10::Int) `hashWithSalt` sv `hashWithSalt` ss `hashWithSalt` tv
    Let bnds body -> s `hashWithSalt` (11::Int) `hashWithSalt` bnds `hashWithSalt` body
    Prim sv ss tv -> s `hashWithSalt` (12::Int) `hashWithSalt` sv `hashWithSalt` ss `hashWithSalt` tv
    Apply body -> s `hashWithSalt` (13::Int) `hashWithSalt` body

instance Show Strat where
  showsPrec d s0 = case s0 of
    Fail ->
      showString "fail"
    Id ->
      showString "id"
    Seq s1 s2 ->
      showParen (d > seq_prec)
        $ showsPrec seq_prec s1
        . showString "; "
        . showsPrec seq_prec s2
    GuardedChoice s1 s2 s3 ->
      showParen (d > choice_prec)
        $ showsPrec (choice_prec+1) s1
        . showString " < "
        . showsPrec (choice_prec+1) s2
        . showString " + "
        . showsPrec (choice_prec+1) s3
    One s ->
      showParen (d > app_prec)
        $ showString "one "
        . showsPrec (app_prec+1) s
    Some s ->
      showParen (d > app_prec)
        $ showString "some "
        . showsPrec (app_prec+1) s
    All s ->
      showParen (d > app_prec)
        $ showString "all "
        . showsPrec (app_prec+1) s
    Match t ->
      showString "?"
        . showsPrec (app_prec+1) t
    Build t ->
      showString "!"
        . showsPrec (app_prec+1) t
    Scope vars s ->
      showString "{ "
      . showString (intercalate "," (map show vars))
      . showString ": "
      . shows s
      . showString " }"
    Call (StratVar f) ss ts ->
     showString (unpack f)
     . showString "("
     . showString (intercalate "," (map show ss))
     . showString "|"
     . showString (intercalate "," (map show ts))
     . showString ")"
    Apply body ->
      showsPrec d body
    Prim (StratVar f) ss ts ->
     showString (unpack f)
     . showString "("
     . showString (intercalate "," (map show ss))
     . showString "|"
     . showString (intercalate "," (map show ts))
     . showString ")"
    Let ss body ->
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
  hashWithSalt s (Strategy svs tvs body) = s `hashWithSalt` svs `hashWithSalt` tvs `hashWithSalt` body

instance Show Closure where
  show (Closure s _) = show s

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

instance (TermVars x,TermVars y) => TermVars (HashMap x y) where
  termVars = termVars . M.toList

instance TermVars Closure where
  termVars (Closure s env) = termVars s <> termVars env

instance TermVars Strat where
  termVars s = case s of
    Fail -> mempty
    Id -> mempty
    Seq s1 s2 -> termVars s1 <> termVars s2
    GuardedChoice s1 s2 s3 -> termVars s1 <> termVars s2 <> termVars s3
    One s1 -> termVars s1
    Some s1 -> termVars s1
    All s1 -> termVars s1
    Match p -> termVars p
    Build p -> termVars p
    Scope vs s1 -> termVars vs <> termVars s1
    Let strats s1 -> termVars strats <> termVars s1
    Call _ _ vs -> termVars vs
    Prim _ _ vs -> termVars vs
    Apply body -> termVars body

instance TermVars StratVar where
  termVars _ = mempty

instance TermVars Strategy where
  termVars (Strategy _ ts s) = termVars ts <> termVars s

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

stratCall :: Prism' Strat Strat
stratCall = L.prism' (\strat -> Apply strat)
                (\s -> case s of
                   Apply strat -> Just strat
                   _ -> Nothing)
